{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import           Types                 as T

import           KodiRPC.Calls
import           KodiRPC.Util
import           KodiRPC.Types.Base
import qualified KodiRPC.Methods.Player as Player
import qualified KodiRPC.Methods.Application as Application
import qualified KodiRPC.Methods.Input as Input
import qualified KodiRPC.Types.Fields.All as All

import           Brick

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Either           as E
import           Data.HashMap.Lazy     as HM
import           Data.List (intercalate)
import           Data.Maybe
import           Data.Monoid
import           Data.Scientific
import           Data.Text             as T hiding (intercalate)
import           Data.Vector           as V (toList, length, null, head)
import           Debug.Trace           as Tr
import           Lens.Micro.Platform   ((%~), (&), (^.), (.~), set, over)
import           Network.Socket        (withSocketsDo)
import           Prelude               as P
import qualified Network.WebSockets    as WS
import           Safe as S

updateSpeed :: Object -> KState -> KState
updateSpeed    p ks = ks & player %~ fmap (speed .~ extractSpeed p)
updatePlayerId p ks = ks & player %~ fmap (playerId .~ extractPlayerId p)
updateTime     p ks = ks & player %~ fmap (timeElapsed .~ extractTime p)
updateVolume   p ks = ks & volume .~ extractVolume p
updateTitle    p ks = ks & title  .~ T.unpack (extractTitle p)

extractPlayerId :: Object -> Int
extractPlayerId p = maybe 0 (ceiling . toRealFloat) $ flip parseMaybe p $ (.:"player") >=> (.:"playerid")
extractSpeed    :: Object -> Float
extractSpeed    p = maybe 0.0 toRealFloat           $ flip parseMaybe p $ (.:"player") >=> (.:"speed")
extractTitle    :: Object -> Text
extractTitle    p = fromMaybe "n/a"                 $ flip parseMaybe p $ (.:"item")   >=> (.:"title")
extractTime     :: Object -> Time
extractTime p     = fromMaybe mempty                $ flip parseMaybe p $ (.:"player") >=> (.:"time") >=> parseJSON
extractVolume   :: Value -> Volume
extractVolume   p = fromMaybe default'              $ parseMaybe parseJSON p
  where default'  = Volume 0 False

sact :: MonadIO m => KState -> Input.Action -> m ()
sact state action = liftIO . void . forkIO . void $ smartAction (state^.k) action

kallState :: KState -> Method -> EventM n (Next KState)
kallState state action = (liftIO . forkIO . void $ kall (state^.k) action) >> continue state

-- populate all the basic kstate data
initKState :: KodiInstance -> IO KState
initKState ki = do
  p     <- getPlayer ki
  vol   <- getVolume ki
  return $ KState ki "window" p vol

getVolume' :: Kaller -> MaybeT IO Volume
getVolume' kaller = MaybeT $ do
  volR <- kaller $ Application.getProperties [Application.Volume, Application.Muted]
  return $ eitherToMaybe volR >>= parseMaybe parseJSON

getVolume :: KodiInstance -> IO Volume
getVolume ki = do
  vol <-  kall ki $ Application.getProperties [Application.Volume, Application.Muted]
  return $ fromRight (Volume 0 False) $ extractVolume <$> vol

timeString :: KState -> String
timeString k = elapsed ++ "/" ++ remaining
  where elapsed = P.show $ maybe mempty _timeElapsed (k ^. player )
        remaining = P.show $ maybe mempty _timeRemaining (k ^. player )

getPlayerId :: KodiInstance -> IO (Either RpcException Int)
getPlayerId ki = do
  res <- kall ki Player.getActivePlayers -- Either e Value
  return $ parsley' =<< res
  where parsley p = flip (withArray "arr") p $ \a ->
          if V.null a then fail "Received no PlayerId"
                      else flip (withObject "Array Object") (V.head a) $ flip (.:) "playerid" :: Parser Int
        unify = ReqException . String . T.pack -- ToDo
        parsley' p = mapLeft unify $ parseEither parsley p

getTimes :: KodiInstance -> IO (Maybe TimeProgress)
getTimes ki = do
  pid <- eitherToMaybe <$> getPlayerId ki
  props <- maybe (pure Nothing) getProps pid
  return $ parseMaybe parseJSON =<< props
    where getProps p = eitherToMaybe <$> kall ki (Player.getProperties p [Player.Time, Player.Totaltime])

getPlayer :: KodiInstance -> IO (Maybe Player)
getPlayer ki = runMaybeT $ do
  pid   <- MaybeT $ eitherToMaybe <$> getPlayerId ki
  props <- MaybeT $ getPProps pid
  let pProps x   = parseMaybe x props
      times      = pProps parseJSON                           :: Maybe TimeProgress
      speed      = pProps $ withObject "Speed" (.:"speed")    :: Maybe Float
      mediatype  = pProps $ withObject "MediaType" (.:"type") :: Maybe String
  media <- MaybeT $ getItem ki pid
  MaybeT . pure $ Player <$> speed <*> Just pid <*> (_elapsed <$> times) <*> (_total <$> times) <*> Just media
    where getPProps pid = eitherToMaybe <$> kall ki (Player.getProperties pid props)
          props         = [Player.Time, Player.Totaltime, Player.Speed, Player.Type]

updatePlayerProps :: KState -> IO KState
updatePlayerProps ki = do
  plyr <- getPlayer (ki^.k)
  return $ ki & player .~ plyr

getItem :: KodiInstance -> Int -> IO (Maybe Media)
getItem ki pid = runMaybeT $ do
  item      <- MaybeT $ eitherToMaybe <$> kall ki (Player.getItem pid fields)
  let md = parseMaybe parseJSON item :: Maybe MediaDetails
  MaybeT . pure $ parseMaybe parseItem' item
    where parseItem = withObject "Item" $ \o -> do
                i <- o.:"item"
                Media <$> (i.:"title") <*> (i.:"file") <*> pure 0
          parseItem' x = parseJSON x <**> parseItem x
          fields = [All.Album, All.Title, All.File, All.Duration, All.Albumartist, All.Artist, All.Displayartist, All.Episode, All.Showtitle, All.Season]

throwYoutube :: KodiInstance -> T.Text -> IO (Either String String)
throwYoutube ki url = do
  putStrLn "Throwing to youtube..."
  let id = Player.matchYouTubeId url
  maybe (pure . Left $ "Could not find video ID") doTheThing id
    where doTheThing vidId = either (Left . show) (const . Right $ "OK") <$> kall ki (Player.openYoutube vidId)

mediaTitle :: Media -> String
mediaTitle (Media title _ _ Movie) = title
mediaTitle (Media title _ _ (Episode episode series season)) = intercalate " - " parts
  where parts = [series, intercalate "x" $ P.map show [season, episode], title]
mediaTitle (Media title _ _ (Audio album _ albumArtist _)) = intercalate " - " parts
  where parts = catMaybes [pure title, headMay albumArtist, pure album]

subTitle :: MediaDetails -> String
subTitle Movie = " "
subTitle (Episode episode series season) = intercalate " - " [seasonXEpisode, series]
  where parts = [seasonXEpisode, series]
        seasonXEpisode = intercalate "x" $ P.map show [season, episode]
subTitle (Audio album _ albumArtist _) = intercalate " - " parts
  where parts = catMaybes [headMay albumArtist, pure album]


safeHead [] = Nothing
safeHead (x:_) = Just x

playerTitle :: Maybe Player -> String
playerTitle = maybe "Nothing Playing" (mediaTitle . _media)

htpc = KodiInstance "192.168.1.4" 8080 "" ""

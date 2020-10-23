{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Types as T

import KodiRPC.Calls
import qualified KodiRPC.Methods.Application as Application
import qualified KodiRPC.Methods.Input as Input
import qualified KodiRPC.Methods.Player as Player
import KodiRPC.Types.Base
import qualified KodiRPC.Types.Fields.All as All
import KodiRPC.Util

import Brick

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Either as E
import Data.HashMap.Lazy as HM
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Data.Scientific
import Data.Text as T hiding (intercalate)
import Data.Vector as V (head, length, null, toList)
import Debug.Trace as Tr
import Lens.Micro.Platform ((%~), (&), (.~), (^.), over, set)
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import Prelude as P
import Safe as S

updateSpeed :: Object -> KState -> KState
updateSpeed p ks = ks & player %~ fmap (speed .~ extractSpeed p)

updatePlayerId p ks = ks & player %~ fmap (playerId .~ extractPlayerId p)

updateTime p ks = ks & player %~ fmap (timeElapsed .~ extractTime p)

updateVolume p ks = ks & volume .~ extractVolume p

updateTitle p ks = ks & title .~ T.unpack (extractTitle p)

extractPlayerId :: Object -> Int
extractPlayerId p =
  maybe 0 (ceiling . toRealFloat) $
  flip parseMaybe p $ (.: "player") >=> (.: "playerid")

extractSpeed :: Object -> Float
extractSpeed p =
  maybe 0.0 toRealFloat $ flip parseMaybe p $ (.: "player") >=> (.: "speed")

extractTitle :: Object -> Text
extractTitle p =
  fromMaybe "n/a" $ flip parseMaybe p $ (.: "item") >=> (.: "title")

extractTime :: Object -> Time
extractTime p =
  fromMaybe mempty $
  flip parseMaybe p $ (.: "player") >=> (.: "time") >=> parseJSON

extractVolume :: Value -> Volume
extractVolume p = fromMaybe default' $ parseMaybe parseJSON p
  where
    default' = Volume 0 False

sact :: MonadIO m => KState -> Input.Action -> m ()
sact state action =
  liftIO . void . forkIO . void $ smartAction (state ^. k) action

kallState :: KState -> Method -> EventM n (Next KState)
kallState state action =
  (liftIO . forkIO . void $ kall (state ^. k) action) >> continue state

-- populate all the basic kstate data
initKState :: KodiInstance -> IO KState
initKState ki = do
  p <- getPlayer ki
  vol <- getVolume ki
  return $ KState ki "window" p vol Playlist

getVolume' :: Monad m => Kaller m -> m (Maybe Volume)
getVolume' kallr = do
  volEither <-
    kallr $ Application.getProperties [Application.Volume, Application.Muted]
  return $ eitherToMaybe volEither >>= (parseMaybe parseJSON)

getVolume :: KodiInstance -> IO Volume
getVolume ki = fromMaybe (Volume 0 False) <$> getVolume' (kall ki)

timeString :: KState -> String
timeString k = elapsed ++ "/" ++ remaining
  where
    elapsed = P.show $ maybe mempty _timeElapsed (k ^. player)
    remaining = P.show $ maybe mempty _timeRemaining (k ^. player)

getPlayerId :: KodiInstance -> IO (Either RpcException Int)
getPlayerId ki = getPlayerId' (kall ki)

getPlayerId' :: Monad m => Kaller m -> m (Either RpcException Int)
getPlayerId' kallr = do
  res <- kallr Player.getActivePlayers -- Either e Value
  return $ parsley' =<< res
  where
    parsley p =
      flip (withArray "arr") p $ \a ->
        if V.null a
          then fail "Received no PlayerId"
          else flip (withObject "Array Object") (V.head a) $
               flip (.:) "playerid" :: Parser Int
    unify = ReqException . String . T.pack -- ToDo
    parsley' p = mapLeft unify $ parseEither parsley p

getTimes' :: Monad m => Kaller m -> PlayerId -> m (Maybe TimeProgress)
getTimes' kallr pid = do
  props <- getProps pid
  return $ parseMaybe parseJSON =<< props
  where
    getProps p =
      eitherToMaybe <$>
      kallr (Player.getProperties p [Player.Time, Player.Totaltime])

getTimes :: KodiInstance -> IO (Maybe TimeProgress)
getTimes ki = do
  pid <- eitherToMaybe <$> getPlayerId ki
  props <- maybe (pure Nothing) getProps pid
  return $ parseMaybe parseJSON =<< props
  where
    getProps p =
      eitherToMaybe <$>
      kall ki (Player.getProperties p [Player.Time, Player.Totaltime])

type PlayerId = Int

getPlayer' ::
     Monad m
  => Kaller m
  -> PlayerId
  -> (Kaller m -> PlayerId -> m (Maybe Media))
  -> m (Maybe Player)
getPlayer' kallr pid itemGetr =
  runMaybeT $ do
    props <- MaybeT $ getPProps pid
    let parseProps x = parseMaybe x props
        times = parseProps parseJSON :: Maybe TimeProgress
        speed = parseProps $ withObject "Speed" (.: "speed") :: Maybe Float
    media <- MaybeT $ itemGetr kallr pid
    MaybeT . pure $
      Player <$> speed <*> Just pid <*> (_elapsed <$> times) <*>
      (_total <$> times) <*>
      Just media
  where
    getPProps pid = eitherToMaybe <$> kallr (Player.getProperties pid props)
    props = [Player.Time, Player.Totaltime, Player.Speed, Player.Type]

doGetPlayer ki pid = getPlayer' (kall ki) pid getItem'

getPlayer :: KodiInstance -> IO (Maybe Player)
getPlayer ki =
  runMaybeT $ do
    pid <- MaybeT $ eitherToMaybe <$> getPlayerId ki
    props <- MaybeT $ getPProps pid
    let pProps x = parseMaybe x props
        times = pProps parseJSON :: Maybe TimeProgress
        speed = pProps $ withObject "Speed" (.: "speed") :: Maybe Float
    media <- MaybeT $ getItem ki pid
    MaybeT . pure $
      Player <$> speed <*> Just pid <*> (_elapsed <$> times) <*>
      (_total <$> times) <*>
      Just media
  where
    getPProps pid = eitherToMaybe <$> kall ki (Player.getProperties pid props)
    props = [Player.Time, Player.Totaltime, Player.Speed, Player.Type]

updatePlayerProps :: KState -> IO KState
updatePlayerProps ki = do
  plyr <- getPlayer (ki ^. k)
  return $ ki & player .~ plyr

getItem :: KodiInstance -> Int -> IO (Maybe Media)
getItem ki pid = getItem' (kall ki) pid

getItem' :: Monad m => Kaller m -> PlayerId -> m (Maybe Media)
getItem' kallr pid =
  runMaybeT $ do
    item <- MaybeT $ eitherToMaybe <$> kallr (Player.getItem pid fields)
    let md = parseMaybe parseJSON item :: Maybe MediaDetails
    MaybeT . pure $ parseMaybe parseMediaAndDetails item
  where
    parseMedia =
      withObject "Item" $ \o -> do
        i <- o .: "item"
        trace (show i) (pure ())
        Media <$> (i .: "title") <*> (i .: "file") <*> pure 0
    parseMediaAndDetails x = parseJSON x <**> parseMedia x
    fields =
      [ All.Album
      , All.Title
      , All.File
      , All.Duration
      , All.Albumartist
      , All.Artist
      , All.Displayartist
      , All.Episode
      , All.Showtitle
      , All.Season
      ]

throwYoutube :: KodiInstance -> T.Text -> IO (Either String String)
throwYoutube ki url = do
  putStrLn "Throwing to youtube..."
  let id = Player.matchYouTubeId url
  maybe (pure . Left $ "Could not find video ID") doTheThing id
  where
    doTheThing vidId =
      either (Left . show) (const . Right $ "OK") <$>
      kall ki (Player.openYoutube vidId)

-- throwYoutube' :: Kodiable -> T.Text -> IO (Either String String)
-- throwYoutube' ki kable url = do
--   putStrLn "Throwing to youtube..."
--   let id = Player.matchYouTubeId url
--   maybe (pure . Left $ "Could not find video ID") doTheThing id
--     where doTheThing vidId = either (Left . show) (const . Right $ "OK") <$> kable ki
mediaTitle :: Media -> String
mediaTitle (Media title _ _ Movie) = title
mediaTitle (Media title _ _ (Episode episode series season)) =
  intercalate " - " parts
  where
    parts = [series, intercalate "x" $ P.map show [season, episode], title]
mediaTitle (Media title _ _ (Audio album _ albumArtist _)) =
  intercalate " - " parts
  where
    parts = catMaybes [pure title, headMay albumArtist, pure album]

subTitle :: MediaDetails -> String
subTitle Movie = " "
subTitle (Episode episode series season) =
  intercalate " - " [seasonXEpisode, series]
  where
    parts = [seasonXEpisode, series]
    seasonXEpisode = intercalate "x" $ P.map show [season, episode]
subTitle (Audio album _ albumArtist _) = intercalate " - " parts
  where
    parts = catMaybes [headMay albumArtist, pure album]

playerTitle :: Maybe Player -> String
playerTitle = maybe "Nothing Playing" (mediaTitle . _media)

htpc = KodiInstance "192.168.1.4" 8080 "" ""

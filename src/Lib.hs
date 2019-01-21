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

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Either           as E
import           Data.HashMap.Lazy     as HM
import           Data.List             (intercalate)
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

updateSpeed :: Object -> KState -> KState
updateSpeed    p ks = ks & player %~ fmap (speed .~ extractSpeed p)
updatePlayerId p ks = ks & player %~ fmap (playerId .~ extractPlayerId p)
updateTime     p ks = ks & player %~ fmap (timeElapsed .~ extractTime p)
updateVolume   p ks = ks & volume .~ extractVolume p
updateTitle    p ks = ks & title  .~ T.unpack (extractTitle p)

updatePlayerProps :: KState -> IO KState
updatePlayerProps ki = fromMaybe ki <$> withMaybe
    where withMaybe = runMaybeT $ do
                      plyr <- MaybeT $ pure (ki^.player) :: MaybeT IO Player
                      let pid = _playerId plyr
                      maybeProps <- MaybeT $ eitherToMaybe <$> kall (ki^.k) (Player.getProperties pid [Player.Time, Player.Totaltime, Player.Speed])
                      speed' <- MaybeT . pure $ parseMaybe (withObject "Speed" (.:"speed")) maybeProps :: MaybeT IO Float
                      elapse <- MaybeT . pure $ parseMaybe (withObject "TimeE" $ (.:"time") >=> parseJSON) maybeProps :: MaybeT IO Time
                      remain <- MaybeT . pure $ parseMaybe (withObject "TimeR" $ (.:"totaltime") >=> parseJSON) maybeProps :: MaybeT IO Time
                      let plyr' = (speed .~ speed') . (timeElapsed .~ elapse) . (timeRemaining .~ remain)
                      return $ ki & player %~ fmap plyr'

extractPlayerId :: Object -> Int
extractPlayerId p = maybe 0 (ceiling . toRealFloat) $ flip parseMaybe p $ (.:"player") >=> (.:"playerid")
extractSpeed    :: Object -> Float
extractSpeed    p = maybe 0.0 toRealFloat $ flip parseMaybe p $ (.:"player") >=> (.:"speed")
extractTitle    :: Object -> Text
extractTitle    p = fromMaybe "n/a" $ flip parseMaybe p $ (.:"item") >=> (.:"title")
extractVolume   :: Value -> Volume
extractVolume   p = fromMaybe def $ parseMaybe parseJSON p
  where def = Volume 0 False
extractTime :: Object -> Time
extractTime p = fromMaybe mempty $ flip parseMaybe p $ (.:"player") >=> (.:"time") >=> parseJSON

sAct :: KState -> Input.Action -> EventM n (Next KState)
sAct state action = suspendAndResume $ do
  result <- forkIO . void $ smartAction test action
  return state

kallState :: KState -> Method -> EventM n (Next KState)
kallState state action = suspendAndResume $ do
  _ <- forkIO . void $ kall (state^.k) action
  return state

-- populate all the basic kstate data
initKState :: KodiInstance -> IO KState
initKState ki = do
  times <- fromMaybe mempty <$> getTimes ki
  p <- getPlayer ki
  vol <- getVolume ki
  return $ KState ki "window" p vol

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
  pid <- MaybeT $ eitherToMaybe <$> getPlayerId ki
  pprops <- MaybeT $ getPProps pid
  times <- MaybeT $ return (parseMaybe parseJSON pprops) :: MaybeT IO TimeProgress
  speed <- MaybeT $ return $ parseMaybe (withObject "Speed" (.: "speed")) pprops :: MaybeT IO Float
  let media = Media "" "" Movie 0
  return $ Player speed pid (_elapsed times) (_total times) media
    where getPProps pid = eitherToMaybe <$> kall ki (Player.getProperties pid [Player.Time, Player.Totaltime, Player.Speed])

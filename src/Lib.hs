{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( someFunc
    ) where

import qualified KodiRPC.Methods.Input as I
import qualified KodiRPC.Methods.Player as P
import qualified KodiRPC.Methods.Player as Player
import qualified KodiRPC.Methods.Application as Application
import qualified KodiRPC.Types.Fields.All as FA
import           KodiRPC.Calls
import           KodiRPC.Types.Base
import           KodiRPC.Util hiding (tdb)

import           Types                 as T

import           Brick
import           Brick.AttrMap
import           Brick.BChan
import           Brick.Types           as BT

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
import           Graphics.Vty          as V
import           Lens.Micro.Platform   ((%~), (&), (^.), (.~), set, over)
import           Network.Socket        (withSocketsDo)
import           Prelude               as P
import qualified Brick.Widgets.Center  as C
import qualified Network.WebSockets    as WS

tdb x = trace (show x) x

app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

topLine ui = hBox [ padRight (Pad 1) . str $ timeString ui
                  , C.hCenter . withAttr attrTitle . str $ maybe "blah" (\p -> p^.media.title) (ui^.player)
                  , padLeft (Pad 2) . str $ "Vol: " ++ show (ui^.volume) ++ "%"
                  ]

midLine ui = hBox [ padLeft (Pad 1) . str $ isPlayingStr ui ]
                  -- album if available, maybe 

drawUI ui = [ topLine ui
            -- , C.hCenterWith (Just '-') $ str "-"
            , C.hCenterWith (Just '-') $ str "-"
            , midLine ui
            ]

someText = attrName "someText"
attrTitle    = attrName "title"

theMap = attrMap V.defAttr [ (someText, fg cyan)
                           , (attrTitle, fg white)
                           ]

-- handleEvent
--      -> handleNotif
--      -> handleVty
--              -> handleKey
--              -> handleChar

handleEvent :: UI -> BrickEvent Name BChanEvent -> EventM Name (Next UI)
handleEvent ui (AppEvent (P.Left x))  = continue $ handleTick ui
handleEvent ui (AppEvent (P.Right x)) = handleNotif ui x
handleEvent ui (VtyEvent x)           = handleVtyEvent ui x
handleEvent ui _                      = continue ui

handleTick :: KState -> KState
handleTick ui = if not . isPlaying $ ui then ui else incTime ui
  where incTime x = x & player %~ fmap (over timeElapsed addSec)

handleNotif :: UI -> Maybe Notif -> EventM Name (Next UI)
handleNotif ui (Just notif) = notifMethodHandler m p ui
  where p = notif^.notifData
        m = notif^.notifMethod :: String
handleNotif ui _        = continue ui

-- possible candidate for monad?
updater fns p ks = P.foldr (\f -> f p) ks fns
updateSpeed    p ks = ks & player %~ fmap (speed .~ extractSpeed p)
updatePlayerId p ks = ks & player %~ fmap (playerId .~ extractPlayerId p)
updateTime     p ks = ks & player %~ fmap (timeElapsed .~ extractTime p)
updateVolume   p ks = ks & volume .~ extractVolume p
updateTitle    p ks = ks & title  .~ T.unpack (extractTitle p)

notifMethodHandler :: String -> Object -> UI -> EventM Name (Next UI)
notifMethodHandler "Player.OnPause" p k = continue $ updater [updatePlayerId, updateSpeed] p k
notifMethodHandler "Player.OnStop" _ k = continue $ k & player .~ Nothing
notifMethodHandler "Player.OnPlay" p k = suspendAndResume $ updatePlayerProps $ (updateSpeed p . updatePlayerId p) k
notifMethodHandler "Player.OnSeek" p k = continue $ (updatePlayerId p . updateSpeed p . updateTime p) k
notifMethodHandler "Application.OnVolumeChanged" p k = continue $ updateVolume (Object p) k

--   -- may have to use suspendandresume here for updating state from IO
--   -- where ks' = ks & nowPlaying .~ /not
-- -- also call to get new player information
-- notifMethodHandler params "Player.OnResume" ks = continue $ ks & player .~ ((ks^.player) & speed .~ 1.0)
notifMethodHandler _ _ ks                 = continue ks

updatePlayerProps :: KState -> IO KState
updatePlayerProps ki = fromMaybe ki <$> withMaybe
    where withMaybe = runMaybeT $ do
                      plyr <- MaybeT $ pure (ki^.player) :: MaybeT IO Player
                      let pid = _playerId plyr
                      maybeProps <- MaybeT $ eitherToMaybe <$> kall (ki^.k) (P.getProperties pid [P.Time, P.Totaltime, P.Speed])
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

handleVtyEvent :: KState -> Event -> EventM n (Next KState)
handleVtyEvent ui (V.EvKey (V.KChar c) []) = handleChar ui c
handleVtyEvent ui (V.EvKey k [])           = handleKey ui k
handleVtyEvent ui _                        = continue ui

handleKey :: KState -> Key -> EventM n (Next KState)
handleKey ui V.KEnter = kallState ui I.select
handleKey ui V.KBS    = kallState ui I.back
handleKey ui _        = continue ui

handleChar :: KState -> Char -> EventM n (Next KState)
-- movement
handleChar ui 'h'  = sAct ui I.Left
handleChar ui 'j'  = sAct ui I.Down
handleChar ui 'k'  = sAct ui I.Up
handleChar ui 'l'  = sAct ui I.Right
-- window ctl
handleChar ui '\t' = kallState ui $ I.executeAction I.Fullscreen
-- media ctl
handleChar ui ' '  = kallState ui $ I.executeAction I.Playpause
handleChar ui 'x'  = kallState ui $ I.executeAction I.Stop
-- else
handleChar ui 'q'  = halt ui
handleChar ui _    = continue ui

test = KodiInstance "localhost" 8080 "" ""
kall' = kall test

sAct state action = suspendAndResume $ do
  result <- forkIO . void $ smartAction test action
  return state

kallState :: UI -> Method -> EventM n (Next UI)
kallState state action = suspendAndResume $ do
  _ <- forkIO . void $ kall (state^.k) action
  return state

-- populate all the basic kstate data
initKState ki = do
  times <- fromMaybe mempty <$> getTimes ki
  p <- getPlayer' ki
  vol <- getVolume ki
  return $ KState ki "window" p vol

getVolume ki = do
  vol <-  kall ki $ Application.getProperties [Application.Volume, Application.Muted]
  return $ fromRight (Volume 0 False) $ extractVolume <$> vol

timeString k = elapsed ++ "/" ++ remaining
  where elapsed = P.show $ maybe mempty _timeElapsed (k ^. player )
        remaining = P.show $ maybe mempty _timeRemaining (k ^. player )

someFunc :: IO ()
someFunc = ping test >>= maybe notGood allGood

allGood :: KodiInstance -> IO ()
allGood ki = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan $ P.Left Tick
    threadDelay 1000000
  forkIO $ forever $ P.Right <$> notification ki >>= writeBChan chan
  initK <- initKState ki
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app initK

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

getPlayer' ki = runMaybeT $ do
  pid <- MaybeT $ eitherToMaybe <$> getPlayerId ki
  pprops <- MaybeT $ getPProps pid
  times <- MaybeT $ return (parseMaybe parseJSON pprops) :: MaybeT IO TimeProgress
  speed <- MaybeT $ return $ parseMaybe (withObject "Speed" (.: "speed")) pprops :: MaybeT IO Float
  let media = Media "" "" Movie 0
  return $ Player speed pid (_elapsed times) (_total times) media
    where getPProps pid = eitherToMaybe <$> kall ki (Player.getProperties pid [Player.Time, Player.Totaltime, Player.Speed])

-- getPlayer ki = do
--   pid <- getPlayerId ki
--   pprops <- either (const . pure $ Nothing) getPProps pid
--   -- aprops <- eitherToMaybe <$> kall ki $ Application.getProperties [Application.Volume, Application.Muted]
--   let times = parseMaybe parseJSON =<< pprops :: Maybe TimeProgress
--       speed = parseMaybe (withObject "Speed" (.: "speed")) =<< pprops :: Maybe Scientific
--       -- volum = parseMaybe parseJSON =<< aprops
--   return times
--     where getPProps pid = eitherToMaybe <$> kall ki (Player.getProperties pid [Player.Time, Player.Totaltime, Player.Speed])

parseMaybeT = MaybeT . parseMaybe

withObject' s = flip $ withObject s

notGood :: IO ()
notGood = void $ putStrLn couldNotConnect
  where couldNotConnect = "Could not connect. Settings correct? Kodi running? Network control enabled?"

justListenToNotifs = forever $ print =<< notification test

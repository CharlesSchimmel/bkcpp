{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           KodiRPC.Methods.Input as I
import           KodiRPC.Calls
import           KodiRPC.Types
import           KodiRPC.Util

import           Types                 as T

import           Brick
import           Brick.AttrMap
import           Brick.BChan
import           Brick.Types           as BT

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Either           as E
import           Data.HashMap.Lazy     as HM
import           Data.List             (intercalate)
import           Data.Maybe
import           Data.Scientific
import           Data.Text             as T hiding (intercalate)
import           Debug.Trace           as Tr
import           Graphics.Vty          as V
import           Lens.Micro.Platform   ((&), (^.), (.~), set, over)
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
                  , C.hCenter . withAttr attrTitle . str $ ui^.T.title
                  , padLeft (Pad 2) . str $ "Vol: " ++ show (ui^.player.volume) ++ "%"
                  ]

midLine ui = hBox [ padLeft (Pad 1) . str $ isPlayingStr ui ]
                  -- album if available, maybe 

drawUI ui = [ topLine ui
            -- , C.hCenterWith (Just '-') $ str "-"
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
handleEvent ui (AppEvent (P.Left x)) = continue $ handleTick ui
handleEvent ui (AppEvent (P.Right x)) = handleNotif ui $ tdb x
handleEvent ui (VtyEvent x) = handleVtyEvent ui $ tdb x
handleEvent ui _            = continue ui

handleTick :: KState -> KState
handleTick ui = if notPlaying then ui else over (player.timeElapsed) addSec ui
  where notPlaying = ui^.player.speed == 0.0

handleNotif :: UI -> Maybe Notif -> EventM Name (Next UI)
handleNotif ui (Just notif) = notifMethodHandler m p ui
  where p = notif^.notifParams
        m = notif^.notifMethod :: String
handleNotif ui _        = continue ui

-- possible candidate for monad?
updater fns p ks = P.foldr (\f -> f p) ks fns
updateSpeed    p ks = ks & (player.speed) .~ extractSpeed p
updatePlayerId p ks = ks & (player.playerId) .~ extractPlayerId p
updateTitle    p ks = ks & title .~ T.unpack (extractTitle p)
updateTime     p ks = ks & (player.timeElapsed) .~ extractTime p
updateVolume   p ks = ks & (player.volume) .~ extractVolume p

notifMethodHandler :: String -> Object -> UI -> EventM Name (Next UI)
notifMethodHandler "Player.OnPause" p k = continue $ updater [updatePlayerId, updateSpeed] p k

notifMethodHandler "Player.OnStop" p k = continue ks''
  where ks' = k & nowPlaying .~ Nothing
        ks'' = ks' & player .~ clearPlayer (ks'^.player)

notifMethodHandler "Player.OnPlay" p k = continue $ updater [updatePlayerId, updateSpeed, updateTitle] p k
  -- also update nowplaying to song/video
  --
notifMethodHandler "Player.OnSeek" p k = continue $ updater [updatePlayerId, updateSpeed, updateTime] p k
notifMethodHandler "Application.OnVolumeChanged" p k = continue $ updater [updateVolume] p k

--   -- may have to use suspendandresume here for updating state from IO
--   -- where ks' = ks & nowPlaying .~ /not
-- -- also call to get new player information
-- notifMethodHandler params "Player.OnResume" ks = continue $ ks & player .~ ((ks^.player) & speed .~ 1.0)
notifMethodHandler _ _ ks                 = continue ks

-- extractPlayerId :: Notif -> Float
extractPlayerId p = maybe 0.0 toRealFloat $ maybeNum $ p #? "data" >#? "player" >#? "playerid"
extractSpeed p    = maybe 0.0 toRealFloat $ maybeNum $ p #? "data" >#? "player" >#? "speed"
extractTitle p    = fromMaybe "n/a" $ maybeStr $ p #? "data" >#? "item" >#? "title"
extractVolume p   = Volume pct mute
  where pct       = truncate $ fromMaybe 0 $ maybeSci <=< maybeNum $ p #? "data" >#? "volume"
        mute      = fromMaybe False $ maybeBool $ p #? "data" >#? "muted"

extractTime p  = Time hrs mins sec mil
  where time   = p #? "data" >#? "player" >#? "time"
        hrs    = safe $ time >#? "hours"
        mins   = safe $ time >#? "minutes"
        sec    = safe $ time >#? "seconds"
        mil    = safe $ time >#? "milliseconds"
        safe t = fromMaybe 0 $ maybeInt <=< maybeNum $ t

handleVtyEvent :: s -> Event -> EventM n (Next s)
handleVtyEvent ui (V.EvKey (V.KChar c) []) = handleChar ui c
handleVtyEvent ui (V.EvKey k [])           = handleKey ui k
handleVtyEvent ui _                        = continue ui

handleKey :: s -> Key -> EventM n (Next s)
handleKey ui V.KEnter = kallState ui select
handleKey ui V.KBS    = kallState ui back
handleKey ui _        = continue ui

handleChar :: s -> Char -> EventM n (Next s)
-- movement
handleChar ui 'h'  = sAct ui I.Left
handleChar ui 'j'  = sAct ui I.Down
handleChar ui 'k'  = sAct ui I.Up
handleChar ui 'l'  = sAct ui I.Right
-- window ctl
handleChar ui '\t' = kallState ui $ xAct I.Fullscreen
-- media ctl
handleChar ui ' '  = kallState ui $ xAct I.Playpause
handleChar ui 'x'  = kallState ui $ xAct I.Stop
-- else
handleChar ui 'q'  = halt ui
handleChar ui _    = continue ui

test = KodiInstance "localhost" 8080
kall' = kall test

sAct state action = suspendAndResume $ do
  result <- smartAction test action
  return state

kallState state action = suspendAndResume $ do
  _ <- kall' action
  return state

-- populate all the basic kstate data
initKState = KState "" p "Play something!" Nothing emptyObject
  where p  = Player 0.0 0.0 te tr (Volume 100 False)
        te = Time 0 12 34 0
        tr = Time 0 45 67 0

timeString kstate = elapsed ++ "/" ++ remaining
  where elapsed   = P.show (plyer ^. timeElapsed)
        remaining = P.show (plyer ^. timeRemaining)
        plyer     = kstate ^. player

xAct = I.executeAction

-- will need another bchan for second ticks
someFunc :: IO ()
-- someFunc = justListenToNotifs
someFunc = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan $ P.Left Tick
    threadDelay 1000000
  forkIO $ forever $ P.Right <$> notification test >>= writeBChan chan
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app initKState

justListenToNotifs = forever $ print =<< notification test

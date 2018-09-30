{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           KodiRPC.Methods.Input as I
import           KodiRPC.Calls
import           KodiRPC.Types

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
import           Lens.Micro.Platform   ((&), (^.), (.~), set)
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
                  , padLeft (Pad 1) . str $ "Vol: " ++ show (ui^.(player.volume)) ++ "%"
                  ]

midLine ui = hBox [ C.hCenter . str $ isPlayingStr ui ]
                  -- album if available, maybe 

drawUI ui = [ topLine ui
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

handleEvent :: UI -> BrickEvent Name (Maybe Notif) -> EventM Name (Next UI)
handleEvent ui (AppEvent x) = handleNotifs ui x
handleEvent ui (VtyEvent x) = handleVtyEvent ui x
handleEvent ui _            = continue ui

handleNotifs :: UI -> Maybe Notif -> EventM Name (Next UI)
handleNotifs ui (Just notif) = trace ( show notif ) $ notifMethodHandler p m ui
  where p = notif^.notifParams
        m = notif^.notifMethod :: String
handleNotifs ui _        = continue ui

(#?) map key = HM.lookup key map
(>#?) map key = map >>= lookup' key
lookup' key (Object map) = HM.lookup key map
lookup' _ _ = Nothing

updateSpeed    params ks = ks & (player.speed) .~ extractSpeed params
updatePlayerId params ks = ks & (player.playerId) .~ extractPlayerId params
updateTitle    params ks = ks & title .~ T.unpack (extractTitle params)

notifMethodHandler :: Object -> String -> UI -> EventM Name (Next UI)
notifMethodHandler params "Player.OnPause"  ks = continue $ updatePlayerId params . updatePlayerId params $ ks
notifMethodHandler params "Player.OnStop"   ks = continue $ updatePlayerId params . updatePlayerId params $ ks'
  where ks' = ks & nowPlaying .~ Nothing
notifMethodHandler params "Player.OnPlay"   ks = continue $ updatePlayerId params . updatePlayerId params $ ks'
  where ks' = updateTitle params ks
--   -- may have to use suspendandresume here for updating state from IO
--   -- where ks' = ks & nowPlaying .~ /not
-- -- also call to get new player information
-- notifMethodHandler params "Player.OnResume" ks = continue $ ks & player .~ ((ks^.player) & speed .~ 1.0)
notifMethodHandler _ _ ks                 = continue ks

-- extractPlayerId :: Notif -> Float
extractPlayerId p = fromMaybe 0.0 $ maybeSci <=< maybeValue $ p #? "data" >#? "player" >#? "playerid"

extractSpeed p = fromMaybe 0.0 $ maybeSci <=< maybeValue $ p #? "data" >#? "player" >#? "speed"

extractTitle p = fromMaybe "n/a" $ maybeStr $ p #? "data" >#? "item" >#? "title"

maybeStr (Just(String s)) = Just s
maybeStr _ = Nothing
maybeValue (Just (Number n)) = Just n
maybeValue _ = Nothing

maybeSci :: Scientific -> Maybe Float
maybeSci s = either Just (const Nothing) $ floatingOrInteger s

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
handleChar ui 'h'  = trace "h" $ sAct ui I.Left
handleChar ui 'j'  = trace "j" $ sAct ui I.Down
handleChar ui 'k'  = trace "k" $ sAct ui I.Up
handleChar ui 'l'  = trace "l" $ sAct ui I.Right
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
  where p  = Player 0.0 0.0 [12,34] [45,67] 0

timeString kstate = elapsed ++ "/" ++ remaining
  where elapsed   = intercalate ":" $ P.map P.show (plyer ^. timeElapsed)
        remaining = intercalate ":" $ P.map P.show (plyer ^. timeRemaining)
        plyer     = kstate ^. player

xAct = I.executeAction

someFunc :: IO ()
someFunc = do
  chan <- newBChan 10
  forkIO $ forever $ notification test >>= writeBChan chan
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app initKState

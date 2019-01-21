{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Types
import Lib

import qualified KodiRPC.Methods.Input as I
import qualified KodiRPC.Methods.Player as Player
import qualified KodiRPC.Methods.Application as Application
import qualified KodiRPC.Types.Fields.All as FA
import           KodiRPC.Calls
import           KodiRPC.Types.Base
import           KodiRPC.Util

import           Brick
import           Brick.AttrMap
import           Brick.BChan
import           Prelude as P
import           Data.Aeson
import           Graphics.Vty          as V
import           Lens.Micro.Platform   ((%~), (&), (^.), (.~), set, over)

-- handleEvent
--      -> handleNotif
--      -> handleVty
--              -> handleKey
--              -> handleChar

handleEvent :: KState -> BrickEvent Name BChanEvent -> EventM Name (Next KState)
handleEvent ui (AppEvent (P.Left x))  = continue $ handleTick ui
handleEvent ui (AppEvent (P.Right x)) = handleNotif ui x
handleEvent ui (VtyEvent x)           = handleVtyEvent ui x
handleEvent ui _                      = continue ui

handleTick :: KState -> KState
handleTick ui = if not . isPlaying $ ui then ui else ui & player %~ fmap (over timeElapsed addSec)

handleNotif :: KState -> Maybe Notif -> EventM Name (Next KState)
handleNotif ui (Just notif) = notifMethodHandler (notif^.notifMethod) (notif^.notifData) ui
handleNotif ui _        = continue ui

notifMethodHandler :: String -> Object -> KState -> EventM Name (Next KState)
notifMethodHandler "Player.OnPause" p k              = continue         $ (updatePlayerId p . updateSpeed p) k
notifMethodHandler "Player.OnStop" _ k               = continue         $ k & player .~ Nothing
notifMethodHandler "Player.OnPlay" p k               = suspendAndResume $ updatePlayerProps $ (updateSpeed p . updatePlayerId p) k
notifMethodHandler "Player.OnSeek" p k               = continue         $ (updatePlayerId p . updateSpeed p . updateTime p) k
notifMethodHandler "Application.OnVolumeChanged" p k = continue         $ updateVolume (Object p) k
-- notifMethodHandler params "Player.OnResume" ks = continue $ ks & player .~ ((ks^.player) & speed .~ 1.0)
notifMethodHandler _ _ ks                 = continue ks

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

isPlaying :: KState -> Bool
isPlaying k = maybe False (\p -> p^.speed > 0) (k^.player)

{-# LANGUAGE OverloadedStrings #-}

module UI where

import Handlers
import Lib
import Types

import           Brick
import           Brick.AttrMap
import           Brick.BChan
import           Brick.Types           as BT
import qualified Brick.Widgets.Center  as C
import Brick.Widgets.Core
import qualified Graphics.Vty          as V
import           Lens.Micro.Platform   ((%~), (&), (^.), (.~), set, over)
import           Data.List (intercalate)


app = App { appDraw         = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

topLine ui = hBox [ padRight (Pad 1) . str $ timeString ui
                  , C.hCenter . withAttr attrTitle . str $ maybe " " (^.media.title) (ui^.player)
                  , padLeft (Pad 2) . str $ "Vol: " ++ show (ui^.volume) ++ "%"
                  ]

midLine ui = hBox [ padRight (Pad 1) . withAttr playPause . str $ isPlayingStr ui 
                  , C.hCenterLayer . withAttr attrTitle . str $ maybe " " (subTitle . (^.media.details)) (ui^.player)
                  , padLeft (Pad 1) . withAttr playPause . str $ "          "
                  ]

headers ui = vBox [ topLine ui
            , midLine ui
            , C.hCenter emptyWidget
            ]

playList ui = emptyWidget

drawUI ui = [headers ui]

someText  = attrName "someText"
attrTitle = attrName "title"
playPause = attrName "playPause"

theMap = attrMap V.defAttr [ (someText, fg V.cyan)
                           , (attrTitle, fg V.white)
                           , (playPause, fg V.white)
                           ]

isPlayingStr :: KState -> String
isPlayingStr k = intercalate "" ["[", status, "]"]
  where status = maybe "stopped" (\p -> if (p^.speed) > 0 then "playing" else "paused") $ k^.player

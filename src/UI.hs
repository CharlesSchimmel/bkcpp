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
import           Graphics.Vty          as V
import           Lens.Micro.Platform   ((%~), (&), (^.), (.~), set, over)


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

isPlayingStr :: KState -> String
isPlayingStr k = maybe "stopped" (\p -> if (p^.speed) > 0 then "playing" else "paused") $ k^.player

module HaskRunner
    ( main
    ) where 

import CodeWorld
import HaskRunner.Core
import HaskRunner.Graphics.Drawers
import HaskRunner.ObjectsHandlers.PlayerHandler

main :: IO ()
main = interactionOf initialWorld timingWorld eventsWorld drawWorld

initialWorld :: Level
initialWorld = Level initialPlayer [] False True 0 1 0 0
  where
    initialPlayer = Player (Bounds
        (Point (-1) 1) 
        (Point 1 1) 
        (Point 1 (-1)) 
        (Point (-1) (-1)))

timingWorld :: Double -> Level -> Level
timingWorld _ level = movePlayer level

eventsWorld :: Event -> Level -> Level
eventsWorld _ level = level

drawWorld :: Level -> Picture
drawWorld = drawLevel

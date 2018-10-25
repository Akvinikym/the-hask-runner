module HaskRunner
    ( main
    ) where 

import CodeWorld
import HaskRunner.Core
import HaskRunner.Graphics.Drawers

main :: IO ()
main = interactionOf initialWorld timingWorld eventsWorld drawWorld

initialWorld :: Level
initialWorld = Level initialPlayer [] False True 0 0 0
  where
    initialPlayer = Player (Bounds
        (Point (-1) 1) 
        (Point 1 1) 
        (Point 1 (-1)) 
        (Point (-1) (-1)))

timingWorld :: Double -> Level -> Level
timingWorld _ level = level

eventsWorld :: Event -> Level -> Level
eventsWorld _ level = level

drawWorld :: Level -> Picture
drawWorld = drawLevel

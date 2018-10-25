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
initialWorld = Level initialPlayer exampleInitialObjects False True 0 0.5 0 0
  where
    initialPlayer = Player (Bounds
        (Point (-1) 1)
        (Point 1 1)
        (Point 1 (-1))
        (Point (-1) (-1))) 0 (-0.5)

    exampleInitialObjects = [
        GameObject (Bounds
            (Point (-5) (-4))
            (Point 5 (-4))
            (Point 5 (-6))
            (Point (-5) (-6))) Platform]
        -- GameObject (Bounds
        --     (Point 9 1)
        --     (Point 11 1)
        --     (Point 11 (-4))
        --     (Point 9 (-4))) Wall,
        -- GameObject (Bounds
        --     (Point (-1) (0))
        --     (Point 1 (0))
        --     (Point 1 (-2))
        --     (Point (-1) (-2))) Spikes,
        -- GameObject (Bounds
        --     (Point (-1) 1)
        --     (Point 1 1)
        --     (Point 1 (-1))
        --     (Point (-1) (-1))) Coin]

timingWorld :: Double -> Level -> Level
timingWorld _ level = movePlayer level

eventsWorld :: Event -> Level -> Level
eventsWorld _ level = level

drawWorld :: Level -> Picture
drawWorld = drawLevel

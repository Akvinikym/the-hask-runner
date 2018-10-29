module HaskRunner where

import CodeWorld
import HaskRunner.Core
import HaskRunner.Graphics.Drawers
import HaskRunner.ObjectsHandlers.PlayerHandler

mainLoop :: IO ()
mainLoop = interactionOf initialWorld timingWorld eventsWorld drawWorld

baseAcceleration = (-0.02)

initialWorld :: Level
initialWorld = Level initialPlayer exampleInitialObjects levelEdges False True 0 baseAcceleration 0 0
  where
    initialPlayer = Player (Bounds
        (Point (-1) 1)
        (Point 1 1)
        (Point 1 (-1))
        (Point (-1) (-1))) 0 0.5

    exampleInitialObjects = [
        GameObject (Bounds
            (Point (-5) (-4))
            (Point 6 (-4))
            (Point 6 (-6))
            (Point (-5) (-6))) Platform]
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
eventsWorld (KeyPress "Up") level
  = level { acceleration = newAcceleration, gravityIsDown = newDirection}
  where
    newDirection = not (gravityIsDown level)
    newAcceleration
      | gravityIsDown level = baseAcceleration
      | otherwise = -baseAcceleration
eventsWorld _ level = level

drawWorld :: Level -> Picture
drawWorld = drawLevel

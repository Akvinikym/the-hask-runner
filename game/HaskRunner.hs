module HaskRunner where

import CodeWorld
import HaskRunner.Core
import HaskRunner.Graphics.Drawers
import HaskRunner.ObjectsHandlers.PlayerHandler
import HaskRunner.ObjectsHandlers.GameObjectsHandler

mainLoop :: IO ()
mainLoop = interactionOf initialWorld timingWorld eventsWorld drawWorld

initialWorld :: Level
initialWorld
    = Level initialPlayer exampleInitialObjects levelEdges False True 0.1 0 0
  where
    initialPlayer = Player (Bounds
        (Point (-1) 1)
        (Point 1 1)
        (Point 1 (-1))
        (Point (-1) (-1))) 0 (-0.5)

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
timingWorld _  = movePlayer . moveObjects

eventsWorld :: Event -> Level -> Level
eventsWorld (KeyPress "F") level
  = level {gravityIsDown = newDirection}
  where
    newDirection = not (gravityIsDown level)
eventsWorld _ level = level

drawWorld :: Level -> Picture
drawWorld = drawLevel

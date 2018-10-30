module HaskRunner where

import CodeWorld
import HaskRunner.Core
import HaskRunner.Graphics.Drawers
import HaskRunner.ObjectsHandlers.PlayerHandler
import HaskRunner.ObjectsHandlers.GameObjectsHandler
import HaskRunner.ObjectsHandlers.LevelHandler

mainLoop :: IO ()
mainLoop = interactionOf initialWorld timingWorld eventsWorld drawWorld

initialWorld :: Level
initialWorld
    = Level
        initialPlayer
        exampleInitialObjects
        levelEdges
        False
        True
        0.1
        0
        0
  where
    initialPlayer = Player (Bounds
        (Point (-1) 1)
        (Point 1 1)
        (Point 1 (-1))
        (Point (-1) (-1))) 0 0

    exampleInitialObjects = [
        GameObject (Bounds
            (Point (-5) (-4))
            (Point 5 (-4))
            (Point 5 (-6))
            (Point (-5) (-6))) Platform,
        GameObject (Bounds
            (Point 3 4)
            (Point 13 4)
            (Point 13 2)
            (Point 3 2)) Platform]
        -- GameObject (Bounds
        --     (Point (3) (1))
        --     (Point 5 (1))
        --     (Point 5 (-1))
        --     (Point (3) (-1))) Spikes]
        -- GameObject (Bounds
        --     (Point (-1) 1)
        --     (Point 1 1)
        --     (Point 1 (-1))
        --     (Point (-1) (-1))) Coin]

timingWorld :: Double -> Level -> Level
timingWorld dt level
    | isFinished level = level
    | otherwise
        = playerDeath
          . (increaseLevelVelocity dt)
          . movePlayer
          . moveObjects $ level

eventsWorld :: Event -> Level -> Level
eventsWorld (KeyPress "F") level
  = level {gravityIsDown = newDirection}
  where
    newDirection = not (gravityIsDown level)
eventsWorld (KeyPress "R") level
    | isFinished level = initialWorld
    | otherwise        = level
eventsWorld _ level = level

drawWorld :: Level -> Picture
drawWorld = drawLevel

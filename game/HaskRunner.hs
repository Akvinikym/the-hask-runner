module HaskRunner where

import CodeWorld
import HaskRunner.Core
import HaskRunner.Graphics.Drawers
import HaskRunner.Generation.Generator
import HaskRunner.ObjectsHandlers.LevelHandler
import HaskRunner.ObjectsHandlers.PlayerHandler
import HaskRunner.ObjectsHandlers.GameObjectsHandler

mainLoop :: IO ()
mainLoop = interactionOf initialWorld timingWorld eventsWorld drawWorld

initialWorld :: Level
initialWorld
    = Level
        initialPlayer
        (objectGenerator 235432)
        levelEdges
        100
        False
        True
        0.1
        0
        0
  where
    initialPlayer = Player (Bounds
        (Point (-1) 3)
        (Point 1 3)
        (Point 1 (1))
        (Point (-1) (1))) 0 0

    exampleInitialObjects = [
        GameObject (Bounds
            (Point (105) (4))
            (Point 109 (4))
            (Point 109 (0))
            (Point 105 (0))) Coin]
        -- GameObject (Bounds
        --     (Point 3 4)
        --     (Point 13 4)
        --     (Point 13 2)
        --     (Point 3 2)) Platform,
        -- GameObject (Bounds
        --     (Point 11 (-4))
        --     (Point 21 (-4))
        --     (Point 21 (-6))
        --     (Point 11 (-6))) Platform,
        -- GameObject (Bounds
        --     (Point 19 4)
        --     (Point 29 4)
        --     (Point 29 2)
        --     (Point 19 2)) Platform,
        -- GameObject (Bounds
        --     (Point 27 (-4))
        --     (Point 37 (-4))
        --     (Point 37 (-6))
        --     (Point 27 (-6))) Platform,
        -- GameObject (Bounds
        --     (Point 35 4)
        --     (Point 45 4)
        --     (Point 45 2)
        --     (Point 35 2)) Platform]
        -- GameObject (Bounds
        --     (Point 15 (0))
        --     (Point 17 (0))
        --     (Point 17 (-2))
        --     (Point 15 (-2))) Spikes]
        -- GameObject (Bounds
        --     (Point (-1) 1)
        --     (Point 1 1)
        --     (Point 1 (-1))
        --     (Point (-1) (-1))) Coin]

timingWorld :: Double -> Level -> Level
timingWorld dt level
    | isFinished level = level
    | otherwise
        = playerDeath . checkCoins
          . (increaseLevelVelocity dt)
          . movePlayer $ level

eventsWorld :: Event -> Level -> Level
eventsWorld (KeyPress " ") level
  = level {gravityIsDown = newDirection}
  where
    newDirection = not (gravityIsDown level)
eventsWorld (KeyPress "R") level
    | isFinished level = initialWorld
    | otherwise        = level
eventsWorld _ level = level

drawWorld :: Level -> Picture
drawWorld = drawLevel

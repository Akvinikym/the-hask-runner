module HaskRunner where

import CodeWorld
import HaskRunner.Core
import HaskRunner.Graphics.Drawers
import HaskRunner.Generation.Generator
import HaskRunner.ObjectsHandlers.LevelHandler
import HaskRunner.ObjectsHandlers.PlayerHandler
import HaskRunner.ObjectsHandlers.GameObjectsHandler

mainLoop :: IO ()
mainLoop = interactionOf
  (initialWorld {state = MainMenu})
  timingWorld
  eventsWorld
  drawWorld

initialWorld :: Level
initialWorld
    = Level
        initialPlayer1
        initialPlayer2
        (objectGenerator 235432)
        levelEdges
        100
        Playing
        0.1
        []
  where
    initialPlayer1 = Player (Bounds
        (Point (-1) 3)
        (Point 1 3)
        (Point 1 (1))
        (Point (-1) (1))) 0 0 True 0

    initialPlayer2 = Player (Bounds
        (Point (-1) (-1))
        (Point 1 (-1))
        (Point 1 (-3))
        (Point (-1) (-3))) 0 0 True 0

    exampleInitialObjects = [
        GameObject (Bounds
            (Point (105) (2))
            (Point 107 (2))
            (Point 107 (0))
            (Point 107 (0))) Coin]
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
timingWorld dt level = case (state level) of
    Playing ->  (increaseLevelVelocity dt)
                . playerDeath __player1
                . playerDeath __player2
                . checkCoins __player1
                . checkCoins __player2
                . movePlayer dt __player1
                . movePlayer dt __player2
                $ level
    _       -> level
  where
    __player1 = player1 level
    __player2 = player2 level

eventsWorld :: Event -> Level -> Level
eventsWorld (KeyPress " ") level
    | (state level) == Playing = newLevel
    | otherwise                = level
  where
    __player1 = player1 level
    newLevel = level {
        player1 = __player1 {gravityIsDown = not (gravityIsDown __player1)} }
eventsWorld (KeyPress ".") level
    | (state level) == Playing = newLevel
    | otherwise                = level
  where
    __player2 = player2 level
    newLevel = level {
        player2 = __player2 {gravityIsDown = not (gravityIsDown __player2)} }
eventsWorld (KeyPress "R") level
    | (state level) == Dead = initialWorld
    | otherwise             = level
eventsWorld (KeyPress "S") level
    | (state level) == MainMenu = initialWorld
    | otherwise                 = level
eventsWorld _ level = level

drawWorld :: Level -> Picture
drawWorld = drawLevel

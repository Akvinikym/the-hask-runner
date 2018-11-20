module GlossRunner where

import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import HaskRunner.Core
import HaskRunner.Graphics.GlossDrawer
import HaskRunner.Generation.Generator
import HaskRunner.ObjectsHandlers.LevelHandler
import HaskRunner.ObjectsHandlers.PlayerHandler
import HaskRunner.ObjectsHandlers.GameObjectsHandler

window :: Display
window = InWindow "HaskRunner" (1280, 1024) (0, 0)

mainLoop :: IO ()
mainLoop = play window white 30 (initialWorld {state = MainMenu}) drawLevel eventsWorld timingWorld

initialWorld :: Level
initialWorld
    = Level
        initialPlayer1
        initialPlayer2
        (objectGenerator 2353)
        levelEdges
        100
        Playing
        0.1
        []
  where
    initialPlayer1 = Player "Player 1"
      (Bounds
        (Point (-1) 3)
        (Point 1 3)
        (Point 1 (1))
        (Point (-1) (1))) 0 0 True 0 0 False

    initialPlayer2 = Player "Player 2"
      (Bounds
        (Point (-1) (-1))
        (Point 1 (-1))
        (Point 1 (-3))
        (Point (-1) (-3))) 0 0 True 0 0 False

eventsWorld :: Event -> Level -> Level
-- eventsWorld _ =
eventsWorld (EventKey (SpecialKey KeySpace) Down _ _) level
  | (state level) == Playing = newLevel
  | otherwise                = level
  where
    __player1 = player1 level
    newLevel = level {
        player1 = __player1 {gravityIsDown = not (gravityIsDown __player1)} }
eventsWorld (EventKey (Char '.') Down _ _) level
  | (state level) == Playing = newLevel
  | otherwise                = level
  where
    __player2 = player2 level
    newLevel = level {
        player2 = __player2 {gravityIsDown = not (gravityIsDown __player2)} }
eventsWorld (EventKey (Char 'r') Down _ _) level
    | (state level) == Dead = initialWorld
    | otherwise             = level
eventsWorld (EventKey (Char 's') Down _ _) level
    | (state level) == MainMenu = initialWorld
    | otherwise                 = level
eventsWorld _ level = level

timingWorld :: Float -> Level -> Level
timingWorld dt level = case (state level) of
    Playing ->  (increaseLevelVelocity (float2Double dt))
                . playersDeaths
                . checkDistances
                . checkCoins __player1
                . checkCoins __player2
                . checkDoors
                . checkButtons __player1
                . checkButtons __player2
                . movePlayer (float2Double dt) __player1
                . movePlayer (float2Double dt) __player2
                $ level
    _       -> level
  where
    __player1 = player1 level
    __player2 = player2 level

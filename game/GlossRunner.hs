module GlossRunner where

import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import HaskRunner.Core
import HaskRunner.Graphics.GlossDrawer
import HaskRunner.Generation.Generator
import HaskRunner.ObjectsHandlers.LevelHandler
import HaskRunner.ObjectsHandlers.PlayerHandler
import HaskRunner.ObjectsHandlers.GameObjectsHandler
import Network.HTTP.Simple
import Data.List
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as A

window :: Display
window = InWindow "HaskRunner" (1280, 1024) (0, 0)

mainLoop :: IO ()
mainLoop = playIO window white 30 (initialWorld {state = MainMenu}) drawLevelIO eventsWorldIO timingWorldIO

drawLevelIO :: Level -> IO Picture
drawLevelIO = return . drawLevel

initialWorld :: Level
initialWorld
    = Level
        initialPlayer1
        initialPlayer2
        (objectGenerator 42353)
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

eventsWorldIO :: Event -> Level -> IO Level
eventsWorldIO event level
  | spaceEvent event = case state level of
      Dead name -> if length name < 5 then return (eventsWorld event level) else uploadScore name level
      _ -> return (eventsWorld event level)
  | escEvent event = case state level of
      Dead name -> return (level {state = MainMenu})
  | otherwise = return (eventsWorld event level)

uploadScore :: String -> Level -> IO Level
uploadScore name level = do
  req <- parseRequest request
  resp <- httpBS req
  return (level {state = MainMenu})
    where
      request = "http://localhost:3000/set_score/" <> name <> "/" <> (show score)
      score = gameScore level (player1 level)

spaceEvent :: Event -> Bool
spaceEvent (EventKey (SpecialKey KeySpace) Down _ _) = True
spaceEvent _ = False

escEvent :: Event -> Bool
escEvent (EventKey (SpecialKey KeyEsc) Down _ _) = True
escEvent _ = False

eventsWorld :: Event -> Level -> Level
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
eventsWorld (EventKey (Char 's') Down _ _) level
    | (state level) == MainMenu = initialWorld
    | otherwise                 = level
eventsWorld (EventKey (Char 'a') Down _ _) level
    | (state level) == MainMenu = onePlayer initialWorld
    | otherwise                 = level
eventsWorld (EventKey (Char 't') Down _ _) level
    | (state level) == MainMenu = level {state = ScoreScreen False []}
    | otherwise                 = level
eventsWorld (EventKey (Char 'a') Down _ _) level
    | isDeadState level     = onePlayer initialWorld
    | otherwise             = level
eventsWorld (EventKey (Char 'e') Down _ _) level
    | isScoreScreen level = initialWorld {state = MainMenu}
    | otherwise                 = level   
eventsWorld (EventKey (Char c) Down _ _) level
  | isDeadState level = addCharToName level c
  | otherwise = level
eventsWorld _ level = level

addCharToName :: Level -> Char -> Level
addCharToName level c = case state level of
  Dead name -> level {state = (Dead newName)}
    where
      newName = if length name < 5 then name ++ [c] else name
  _ -> level


isScoreScreen :: Level -> Bool
isScoreScreen level = case state level of
  ScoreScreen _ _ -> True
  _ -> False

isDeadState :: Level -> Bool
isDeadState level = case state level of
  Dead _ -> True
  _ -> False

timingWorldIO :: Float -> Level -> IO Level
timingWorldIO dt level = case (state level) of
  ScoreScreen False _ -> do
    resp <- httpBS "http://localhost:3000/score"
    let scores = parseResponse (BSL.fromStrict (getResponseBody resp))
    let topScores = ((take 10) . reverse . (sortOn score)) scores
    return level {state = ScoreScreen True topScores}
      where
        parseResponse resp = case A.decode resp :: Maybe [Score] of
          Nothing -> []
          Just a -> a
  _ -> return (timingWorld dt level)


timingWorld :: Float -> Level -> Level
timingWorld dt level = case (state level) of
    Playing ->  (increaseLevelVelocity (float2Double dt))
                . playersDeaths
                . checkPlayerDeath __player1
                . checkPlayerDeath __player2
                . checkDistances
                . checkDoors
                . checkButtons __player1
                . checkButtons __player2
                . checkCoins __player1
                . checkCoins __player2
                . movePlayer (float2Double dt) __player1
                . movePlayer (float2Double dt) __player2
                $ level
    _       -> level
  where
    __player1 = player1 level
    __player2 = player2 level



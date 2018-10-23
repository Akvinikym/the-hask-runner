module HaskRunner.Core where

-- | Contains general-purpose data types and functions

-- mainLoop :: Renderer -> IO ()
-- mainLoop renderer = do
    -- acceptInput
    -- drawNextScene

-- possible menu choice
data Menu = NewGame | FinishGame

-- current level state
data Level = Level
    { player        :: Player
    -- , map           :: Map
    , score         :: Int
    , isFinished    :: Bool
    , gravityVector :: Bool    -- ^ true, if gravity is upside-down
    }

-- rectangular bounds of the object
data Bounds = Bounds
    { topLeft     :: Double
    , bottomRight :: Double
    } deriving (Show)

-- player of the game
data Player = Player Bounds

type Distance = Double

currentScore :: Distance -> Int -> Int
currentScore _ _ = 0

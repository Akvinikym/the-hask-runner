{-# LANGUAGE DeriveGeneric #-}
module HaskRunner.Core where

import GHC.Generics
import qualified Data.Aeson as A
-- | Contains general-purpose data types and functions

-- size of the playable area
screenHeight :: Double
screenHeight = 12
screenWidth :: Double
screenWidth = 20

-- value by which vertical speed is adjusted
baseGravity :: Double
baseGravity = (-0.3)

-- how fast the level accelerates
horizontalAcceleration :: Double
horizontalAcceleration = 0.02

-- current level state; main state of the world as well
data Level = Level
    { player1       :: Player     -- ^ first player
    , player2       :: Player     -- ^ second player
    , levelMap      :: Map        -- ^ collection of current game objects
    , edges         :: Map        -- ^ collection of game borders
    , levelPos      :: Double     -- ^ position of the level screen
    , state         :: GameState  -- ^ current state of the game
    , horVelocity   :: Velocity   -- ^ world's horizontal velocity
    , doorsOpened   :: [Double]   -- ^ opened doors ids
    } deriving (Show)

data GameState =
    MainMenu
    | Playing
    | Dead String
    | ScoreScreen Bool [Score]
    deriving (Show, Eq)

-- player of the game
data Player = Player
  {  name          :: String    -- ^ player's name
  ,  pbounds       :: Bounds    -- ^ players position
  ,  pHorVelocity  :: Velocity  -- ^ player's horizontal velocity
  ,  pVertVelocity :: Velocity  -- ^ player's vertical velocity
  ,  gravityIsDown :: Bool      -- ^ true, if gravity is upside-down
  ,  lilcoins      :: Int       -- ^ number of coins player collected
  ,  distance      :: Int       -- ^ distance the player travelled
  ,  isDead        :: Bool      -- ^ is the player dead?
  } deriving (Show)

instance Eq Player where
    p1 == p2 = name p1 == name p2


-- rectangular bounds of the object
data Bounds = Bounds
    { topLeft     :: Point
    , topRight    :: Point
    , bottomRight :: Point
    , bottomLeft  :: Point
    } deriving (Show, Eq)

-- move the bounds to some direction: for example,
-- (-1, -1) will move the bounds left-bottom to 1 point
-- (1, 1)   will move the bounds right-up to 1 point
-- note: to be wrapped into high-level function, such as moveSomeObject
moveBounds :: Bounds -> (Double, Double) -> Bounds
moveBounds
    (Bounds (Point x1 y1) (Point x2 y2) (Point x3 y3) (Point x4 y4)) (x, y)
    = Bounds (Point (x1 + x) (y1 + y)) (Point (x2 + x) (y2 + y))
      (Point (x3 + x) (y3 + y)) (Point (x4 + x) (y4 + y))

-- find the center of the bounds rectangle
boundsCenter :: Bounds -> Point
boundsCenter (Bounds (Point x1 y1) _ (Point x2 y2) _)
    = Point ((x1 + x2) / 2) ((y1 + y2) / 2)

-- find width and height of the bounds rectangle
boundsWidthHeight :: Bounds -> (Double, Double)
boundsWidthHeight (Bounds (Point x1 y1) _ (Point x2 y2) _)
    = (x2 - x1, y1 - y2)

-- find right-most and left-most X coordinates of the bounds
boundsLeftRightCoords :: Bounds -> (Double, Double)
boundsLeftRightCoords bounds = (leftMost, rightMost)
  where
    Point leftMost _  = topLeft bounds
    Point rightMost _ = topRight bounds


data Point = Point Double Double
    deriving (Show, Eq)

type Map = [GameObject]
type Velocity = Double

-- | Different generated objects: platforms or obsctacles

instance Eq GameObject where
    (GameObject (Bounds (Point x _) _ _ _) _) ==  (GameObject (Bounds (Point x' _) _ _ _) _) = x == x'

instance Ord GameObject where
    (GameObject (Bounds (Point x _) _ _ _) _) `compare` (GameObject (Bounds (Point x' _) _ _ _) _) = x `compare` x'

-- object, representing generated element of the game
data GameObject = GameObject
    { bounds       :: Bounds
    , objectType   :: ObjectType
    }  deriving (Show)



-- type of what can be generated
data ObjectType =
    Platform    -- ^ rectangular platform, on which player can stand
    | Wall      -- ^ borders of the game
    | Spikes    -- ^ death-bringing obstacle
    | Coin      -- ^ source of additional points
    | Button Double   -- ^ opens doors
    | Door   Double         -- ^ openable platform
    deriving (Eq, Show)

-- if collision with object causes death
deadObject :: GameObject -> Bool
deadObject (GameObject _ objType)
    = objType == Wall || objType == Spikes

-- find out, if the object is on the screen
onScreen :: Level -> GameObject -> Bool
onScreen level obj
    | leftMost < (screenWidth + (levelPos level)) &&
      rightMost > (-screenWidth + (levelPos level)) = True
    | otherwise                                     = False
  where
    (leftMost, rightMost) = boundsLeftRightCoords (bounds obj)


-- edges of the level represented by walls
levelEdges :: Map
levelEdges = [bottomWall, leftWall, upWall]
  where
    bottomWall = GameObject (Bounds
        (Point (- screenWidth + 1) (- screenHeight + 3))
        (Point (screenWidth - 1) (- screenHeight + 3))
        (Point (screenWidth - 1) (- screenHeight + 2))
        (Point (- screenWidth + 1) (- screenHeight + 2))) Wall
    leftWall = GameObject (Bounds
        (Point (- screenWidth + 1) (screenHeight - 3))
        (Point (- screenWidth + 2) (screenHeight - 3))
        (Point (- screenWidth + 2) (- screenHeight + 2))
        (Point (- screenWidth + 1) (- screenHeight + 2))) Wall
    upWall = GameObject (Bounds
        (Point (- screenWidth + 1) (screenHeight - 2))
        (Point (screenWidth - 1) (screenHeight - 2))
        (Point (screenWidth - 1) (screenHeight - 3))
        (Point (- screenWidth + 1) (screenHeight - 3))) Wall

-- calculate total score of the player
gameScore :: Level -> Player -> Integer
gameScore _ player
    = toInteger (distance player - 100 + (10 * (lilcoins player)))

objectsOnScreen :: Level -> [GameObject]
objectsOnScreen level
  = take 50
    (dropWhile (not . (onScreen level)) (levelMap level))

data Score = Score
  { pname :: String
  , score :: Integer
  } deriving (Generic, Show, Eq)

instance A.ToJSON Score where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.FromJSON Score

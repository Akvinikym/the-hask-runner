module HaskRunner.Core where

-- | Contains general-purpose data types and functions

-- size of the playable area
screenSize :: (Double, Double)
screenSize = (20, 12)

-- current level state; main state of the world as well
data Level = Level
    { player        :: Player     -- ^ player of the game
    , levelMap      :: Map        -- ^ collection of current game objects
    , edges         :: Map        -- ^ collection of game borders
    , isFinished    :: Bool       -- ^ collision with obstacle occured
    , gravityIsDown :: Bool       -- ^ true, if gravity is upside-down
    , horVelocity   :: Velocity   -- ^ world's horizontal velocity
    , acceleration  :: Double     -- ^ current acceleration caused by the gravity
    , distance      :: Distance   -- ^ distance player travelled so far
    , lilcoins      :: Int        -- ^ number of coins player collected
    }


-- player of the game
data Player = Player
  {  pbounds :: Bounds         -- ^ players position
  ,  pHorVelocity :: Velocity  -- ^ player's horizontal velocity
  ,  pVertVelocity :: Velocity -- ^ player's vertical velocity
  }


-- rectangular bounds of the object
data Bounds = Bounds
    { topLeft     :: Point
    , topRight    :: Point
    , bottomRight :: Point
    , bottomLeft  :: Point
    } deriving (Show)

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
    = (x2 - x1, y2 - y1)


data Point = Point Double Double
    deriving (Show)

type Map = [GameObject]
type Distance = Double
type Velocity = Double


-- | Different generated objects: platforms or obsctacles

-- object, representing generated element of the game
data GameObject = GameObject
    { bounds       :: Bounds
    , objectType   :: ObjectType
    }

-- type of what can be generated
data ObjectType =
    Platform    -- ^ rectangular platform, on which player can stand
    | Wall
    | Spikes 
    | Coin

levelEdges :: Map
levelEdges = [bottomWall, leftWall, upWall]
  where
    (screenWidth, screenHeight) = screenSize
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

module HaskRunner.Core where

-- | Contains general-purpose data types and functions

-- current level state; main state of the world as well
data Level = Level
    { player        :: Player     -- ^ player of the game
    , map           :: Map        -- ^ collection of current game objects
    , isFinished    :: Bool       -- ^ collision with obstacle occured
    , gravityIsDown :: Bool       -- ^ true, if gravity is upside-down
    , horVelocity   :: Velocity   -- ^ current player's (or world's) 
                                  --   horizontal velocity
    , vertVelocity  :: Velocity   -- ^ current player's vertical velocity
    , distance      :: Distance   -- ^ distance player travelled so far
    , lilcoins      :: Int        -- ^ number of coins player collected
    }

-- player of the game
data Player = Player Bounds

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
moveBounds :: Bounds -> (Double, Double) -> Bounds
moveBounds 
    (Bounds (Point x1 y1) (Point x2 y2) (Point x3 y3) (Point x4 y4)) (x, y)
    = Bounds (Point (x1 + x) (y1 + y)) (Point (x2 + x) (y2 + y))
      (Point (x3 + x) (y3 + y)) (Point (x4 + x) (y4 + y))

data Point = Point Double Double 
    deriving (Show)

type Map = [GameObject]
type Distance = Double
type Velocity = Double

-- object, representing generated elements of the game
data GameObject = GameObject 
    { bounds       :: Bounds
    , objectType   :: ObjectType
    } 

-- type of what can be generated
data ObjectType = Wall | Spikes | Coin

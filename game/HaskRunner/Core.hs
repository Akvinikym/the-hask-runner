module HaskRunner.Core where

-- | Contains general-purpose data types and functions

-- current level state; main state of the world as well
data Level = Level
    { player        :: Player     -- ^ player of the game
    , map           :: Map        -- ^ collection of current game objects
    , isFinished    :: Bool       -- ^ collision with obstacle occured
    , gravityVector :: Bool       -- ^ true, if gravity is upside-down
    , velocity      :: Velocity   -- ^ current player's (or world's) velocity
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

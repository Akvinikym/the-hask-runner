module HaskRunner.Generation.Generator where

import HaskRunner.Core

type Seed = Double

-- generate objects based on some random seed
mapGenerator :: Seed -> [GameObject]
mapGenerator _ = []

-- object, representing generated elements of the game
data GameObject = GameObject 
    { bounds       :: Bounds
    , objectType   :: ObjectType
    } 

-- type of what can be generated
data ObjectType = Wall | Spikes | Coin

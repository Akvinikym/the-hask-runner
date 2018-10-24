module HaskRunner.Generation.Generator where

import HaskRunner.Core

type Seed = Double

{-|
Generate infinite list if objects from random seed
Example use in drawing: 
    drawGame :: Game -> Picture
    drawGame = drawGates . gameGates
-}
objectGenerator :: Seed -> [GameObject]
objectGenerator _ = []

-- Element of the game
data GameObject = GameObject 
    { bounds       :: Bounds
    , objectType   :: ObjectType
    } 

data ObjectType = Wall | Spikes | Coin

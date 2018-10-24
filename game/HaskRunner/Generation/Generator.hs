module HaskRunner.Generation.Generator where

import HaskRunner.Core
import Data.Graph

type Seed = Double

-- x0, x1, x2, x3
type Pgram =  (Double, Double, Double, Double)

PLAYER_SIZE = (Double, Double)

{-|
Generate infinite list if objects from random seed
Example use in drawing: 
    drawGame :: Game -> Picture
    drawGame = drawObjects . objectGenerator
-}
objectGenerator :: Seed -> [GameObject]
objectGenerator _ = startObjects ++ []

-- Objects at the start of the level
startObjects :: [GameObject]
startObjects = []

-- Element of the game
data GameObject = GameObject 
    { bounds       :: Bounds
    , objectType   :: ObjectType
    } 

data ObjectType = Wall | Spike | Coin

-- Infinite list of gameObj batches
levelGenerator :: Seed -> [[GameObject]]
levelGenerator s = _

-- Randomly generate walls
--  1. Consider player size
--  2. Consider player speed (?)
--  3. Consider previous walls (?) (min delta between walls)
generateRandomWalls :: Seed -> [Wall]
generateRandomWalls s = _

-- Generate graph with walls as vertices and paths inbetween as edges
makeGraph :: [Wall] -> Graph
makeGraph walls = _

-- Get zone, represented by 2 Pgrams, from every point of which player could
-- reach the wall passed
getInPaths :: Wall -> [Pgram]
getInPaths w = _

-- Get zone, represented by 2 Pgrams, which shows every point player could reach
-- from this wall
getOutPaths :: Wall -> [Pgram]
getOutpaths w = _

-- Get intersection of two paralellograms
intersectPgrams :: Pgram -> Pgram -> Pgram
intersectPaths p p' = _

-- Get player horizontal and vertical speed from distance passed
calculateSpped :: Double -> (Double, Double)
calculateSpped x = _

-- Check whether player can reach second wall from first wall
checkIntersection :: Wall -> Wall -> Bool
checkIntersection w w' = _

-- Check if graph contains feasible route from start to end
checkGraph :: Graph -> Bool
checkGraph g = _

-- Add spikes to reduce # of paths through graph or make path harder
backwardGraphPass :: Graph -> [Spike]
backwardGraphPass g = _

-- Generate batch of gameObjects
batchGenerator :: Seed -> [GameObject]
batchGenerator s = _
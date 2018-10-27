module HaskRunner.Generation.Generator where

import HaskRunner.Core
import Data.Graph

type Seed = Double

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
getInPaths :: Wall -> [Bounds]
getInPaths w = _

-- Get zone, represented by 2 Pgrams, which shows every point player could reach
-- from this wall
getOutPaths :: Wall -> [Bounds]
getOutpaths w = _

-- Get intersection of two trapezioidal bounds
intersectTrapBounds :: Bounds -> Bounds -> Maybe Bounds
intersectTrapBounds b b' = b''
    where
        x1, y1 = topLeft b
        x2, y2 = topRight b
        x3, y3 = bottomRight b
        x4, y4 = bottomLeft b
        x1', y1' = topLeft b'
        x2', y2' = topRight b'
        x3', y3' = bottomRight b'
        x4', y4' = bottomLeft b'
        y_up = min y1 y1'
        y_low = max y4 y4'
        -- TODO Contains logic
        x1'' = (y_up - y4)*(x2 - x3)/(y2 - y4) + x3
        y1'' = y_up
        x2'' = x2
        y2'' =  y2
        x3'' = x1' - (x1' - x4')*(y1' - y_low)/(y1' - y4')
        y3'' = y_low
        x4'' = x4'
        y4'' = y4'
        -- TODO Check if generated intersection is really going from first to second
        b'' = Bounds (Point x1'' y1'') (Point x2'' y2'') (Point x3'' y3'') (Point x4'' y4'') 


-- Get player horizontal and vertical speed from distance passed
calculateSpeed :: Double -> (Double, Double)
calculateSpeed x = (horizontalSpeed, verticalSpeed)
    where
        initialSpeed = 2    -- TODO: take from Core
        acceleration = 0.2  -- TODO: take from Core
        verticalSpeed = 4   -- TODO: take from Core
        horizontalSpeed = maximum(tPath(acceleration / 2, initialSpeed, -x))
        tPath (a, b, c) = [t1, t2]
            where
                e = -b (2 * a)
                d = b * b - 4 * a * c
                t1 = e + sqrt d / (2 * a)
			    t2 = e - sqrt d / (2 * a)
        
                
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

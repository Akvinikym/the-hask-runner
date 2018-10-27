module HaskRunner.Generation.Generator where

import Data.Graph hiding (Bounds)	
import Control.Monad
import HaskRunner.Core


type Seed = Double


-- {-|
-- Generate infinite list if objects from random seed
-- Example use in drawing: 
--     drawGame :: Game -> Picture
--     drawGame = drawObjects . objectGenerator
-- -}
-- objectGenerator :: Seed -> [GameObject]
-- objectGenerator _ = startObjects ++ []

-- -- Objects at the start of the level
-- startObjects :: [GameObject]
-- startObjects = []

-- -- Infinite list of gameObj batches
-- levelGenerator :: Seed -> [[GameObject]]
-- levelGenerator s = _

-- -- Randomly generate walls
-- --  1. Consider player size
-- --  2. Consider player speed (?)
-- --  3. Consider previous walls (?) (min delta between walls)
-- generateRandomWalls :: Seed -> [GameObject]
-- generateRandomWalls s = _

-- -- Generate graph with walls as vertices and paths inbetween as edges
-- makeGraph :: [GameObject] -> Graph
-- makeGraph walls = _

-- -- Get zone, represented by 2 Pgrams, from every point of which player could
-- -- reach the wall passed
-- getInPaths :: GameObject -> [Bounds]
-- getInPaths w = _

-- -- Get zone, represented by 2 Pgrams, which shows every point player could reach
-- -- from this wall
-- getOutPaths :: GameObject -> [Bounds]
-- getOutpaths w = _

-- Get intersection of two trapezioidal bounds
-- TODO change to intersectTrapBounds :: Bounds -> Bounds -> Maybe Bounds
intersectTrapBounds :: Bounds -> Bounds -> Maybe Bounds
intersectTrapBounds b b' = b''
    where
        Point _ y1 = topLeft b
        Point x2 y2 = topRight b
        Point x3 _ = bottomRight b
        Point _ y4 = bottomLeft b
        Point x1' y1' = topLeft b'
        Point x4' y4' = bottomLeft b'
        y_up = min y1 y1'
        y_low = max y4 y4'
        isCollinear = (y1 == y1') || (y4 == y4')
        x1'' = (y_up - y4)*(x2 - x3)/(y2 - y4) + x3
        y1'' = y_up
        x3'' = x1' - (x1' - x4')*(y1' - y_low)/(y1' - y4')
        y3'' = y_low
        b'' = if isCollinear then Nothing else Just $ Bounds (Point x1'' y1'') (Point x2 y2) (Point x3'' y3'') (Point x4' y4') 

-- Get player horizontal and vertical speed from distance passed
calculateSpeed :: Double -> (Double, Double)
calculateSpeed x = (horizontalSpeed, verticalSpeed)
    where
        initialSpeed = 2    -- TODO: take from Core
        acceleration = 0.2  -- TODO: take from Core
        verticalSpeed = 4   -- TODO: take from Core
        horizontalSpeed = x / tPath(acceleration / 2, initialSpeed, -x)
        tPath (a, b, c) = t1
            where
                e = -b * (2 * a)
                d = b * b - 4 * a * c
                t1 = e + sqrt (d / (2 * a))
        
                
-- -- Check whether player can reach second wall from first wall
-- checkIntersection :: GameObject -> GameObject -> Bool
-- checkIntersection w w' = _

-- -- Check if graph contains feasible route from start to end
-- checkGraph :: Graph -> Bool
-- checkGraph g = _

-- -- Add spikes to reduce # of paths through graph or make path harder
-- backwardGraphPass :: Graph -> [GameObject]
-- backwardGraphPass g = _

-- -- Generate batch of gameObjects
-- batchGenerator :: Seed -> [GameObject]
-- batchGenerator s = _

module HaskRunner.Generation.Generator where

import Data.Graph hiding (Bounds)
-- import Control.Monad
import HaskRunner.Core
import System.Random
-- import Data.Random
type Seed = Double



-- Helper function for phi_inverse
rational_approx :: Double -> Double
rational_approx t = t - ((0.010328*t + 0.802853)*t + 2.515517) / 
               (((0.001308*t + 0.189269)*t + 1.432788)*t + 1.0);

-- Inverse of normal CDF
phi_inverse :: Double -> Double
phi_inverse p = 
    if p < 0.5
        then  - rational_approx( sqrt (-2.0*log(p)) )
        else  rational_approx( sqrt (-2.0*log(1.0 - p)) )

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

-- Infinite list of gameObj batches
levelGenerator :: Int -> [[GameObject]]
levelGenerator s = map generateRandomWalls seedRvs
    where 
        g = mkStdGen s
        seedRvs =  (randoms g :: [Int])

platformHeight = 1.1

makeWall :: Double -> Double -> Double -> GameObject
makeWall x y l = GameObject bounds Platform
    where 
        bounds = Bounds p1 p2 p3 p4
        p1 = Point x y
        p2 = Point (x + l) y
        p3 = Point (x + l) (y - platformHeight)
        p4 = Point x (y - platformHeight)

-- -- Randomly generate walls
-- --  1. Consider player size (Done)
-- --  2. Consider player speed (?)
-- --  3. Consider previous walls (?) (min delta between walls)
generateRandomWalls :: Int -> [GameObject]
generateRandomWalls s = zipWith3 makeWall platformXOrigins platformYOrigins platformLenghts
        where 
            meanNumberOfWalls = 30.0
            screenHeight = 50.0
            meanOriginOffset = meanWallLength / 3.0
            meanWallLength = 10.0
            playerHeight = 2.0
            yLevels :: Int
            yLevels = floor (screenHeight / (playerHeight * 4.0))
            normalRvs = map phi_inverse (randomRs (0.0, 1.0) (mkStdGen s))
            uniformRvs = randomRs (0, yLevels) (mkStdGen s)
            numberOfWalls = round (meanNumberOfWalls * (head normalRvs))
            platformLenghts = map (meanWallLength *) (take numberOfWalls (drop 1 normalRvs))
            platformYOrigins = map (\t -> (playerHeight * 4.0) * (fromIntegral t)) (take numberOfWalls uniformRvs)
            platformXOrigins = scanl (+) 0 (map (meanOriginOffset * ) (take numberOfWalls (drop (1 + numberOfWalls) normalRvs)))


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

-- This method should return difference from first bound along left side of second bound
-- substractTrapBounds :: Bounds -> Bounds -> Maybe Bounds

-- Get intersection of two trapezioidal bounds
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
        horizontalSpeed = x / timeFromX(acceleration / 2, initialSpeed, x)
        timeFromX (a, b, c) = t1
            where
                e = -b * (2 * a)
                d = b * b - 4 * a * (-c)
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

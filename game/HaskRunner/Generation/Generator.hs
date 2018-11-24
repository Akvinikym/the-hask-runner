module HaskRunner.Generation.Generator where

import qualified Data.Graph as G
import HaskRunner.Core
import System.Random
import Data.Maybe
import Data.List
type Seed = Double

-- | Constants
platformHeight = 0.8
verticalSpeed = 4
initialSpeed = 2
playerHeight = 2.0
meanNumberOfWalls = 10
minNumberOfWalls = 4
minWallY = 3.5 - screenHeight 
maxWallY = screenHeight - 3.5 
yLevels :: Int
yLevels = 5
meanOriginOffset = 1.5
baseOriginOffset = 7.33
wallBase = 7.0
meanWallLength = 1.0
meanNumberOfSpikes :: Int
meanNumberOfSpikes = 5
-- |

-- Helper function for phi_inverse
rational_approx :: Double -> Double
rational_approx t = t - ((0.010328*t + 0.802853)*t + 2.515517) /
               (((0.001308*t + 0.189269)*t + 1.432788)*t + 1.0);

-- Inverse of normal CDF
phi_inverse :: Double -> Double
phi_inverse p | p < 0.5 = - rational_approx( sqrt (-2.0*log(p)))
              | otherwise = rational_approx( sqrt (-2.0*log(1.0 - p)))

objectGenerator :: Int -> [GameObject]
objectGenerator s = concat (levelGenerator s)


-- Objects at the start of the level
-- TODO Change to lowest possible platform and top platform
safeZone :: Double -> [GameObject]
safeZone xOrigin = [ GameObject (Bounds
        (Point xOrigin (screenHeight - 2.0))
        (Point (xOrigin + 10) (screenHeight - 2.0))
        (Point (xOrigin + 10) (screenHeight - 3.5))
        (Point xOrigin (screenHeight - 3.5))) Platform,

    GameObject (Bounds
        (Point xOrigin (3.5 - screenHeight ))
        (Point (xOrigin + 10) (3.5 - screenHeight ))
        (Point (xOrigin + 10) (2 - screenHeight  ))
        (Point xOrigin (2 - screenHeight  ))) Platform ]

-- Infinite list of gameObj batches
levelGenerator :: Int -> [[GameObject]]
levelGenerator s = scanl getNextXOrigin (safeZone 0.0) objectsMix
    where
        seedRvs =  (randoms (mkStdGen s) :: [Int])
        walls = map generateRandomWalls seedRvs 
        spikes = map generateRandomSpikes seedRvs  
        verticalWalls = map generateRandomVerticalWalls seedRvs 
        doors = map generateRandomDoors seedRvs

        zippedBatches = zip4 verticalWalls walls spikes doors
        objectsMix = map (\ (x1, x2, x3, x4) t -> merge [(x1 t), (x2 t), (x3 t), (x4 t)]) zippedBatches
        getNextXOrigin prev next =  (safeZone x) ++ [makeCoin (x + 10.0) 0.0] ++ next (x + 10.0)
            where
                Point x _ = topRight (bounds (last prev))


makeVerticalWall :: Double -> Double -> Double -> GameObject
makeVerticalWall x y l = GameObject bounds Platform
    where
        bounds = Bounds p1 p2 p3 p4
        p1 = Point x (y + l - 1)
        p2 = Point (x + platformHeight) (y + l - 1)
        p3 = Point (x + platformHeight)  y
        p4 = Point x y

makeWall :: Double -> Double -> Double -> GameObject
makeWall x y l = GameObject bounds Platform
    where
        bounds = Bounds p1 p2 p3 p4
        p1 = Point x y
        p2 = Point (x + l) y 
        p3 = Point (x + l) (y - platformHeight)
        p4 = Point x (y - platformHeight) 

merge :: Ord a => [[a]] -> [a]
merge  = sort . concat 

merge2 :: Ord a => [a] -> [a] -> [a]
merge2 [] xs = xs
merge2 xs [] = xs
merge2 (x : xs) (x' : xs') |  x < x' = [x] ++ merge2 xs ([x'] ++ xs')
                           | otherwise = [x'] ++ merge2 ([x] ++ xs) xs'

getFeasibleRandomWalls :: Int -> Double -> [GameObject]
getFeasibleRandomWalls s xOrigin = head $ dropWhile (not.isFeasible) (map (\s -> generateRandomWalls s xOrigin) seeds)
        where
            seeds =  (randoms (mkStdGen s) :: [Int])

-- -- Randomly generate walls
generateRandomWalls :: Int -> Double -> [GameObject]
generateRandomWalls s xOrigin = zipWith3 makeWall platformXOrigins platformYOrigins platformLenghts
        where
            normalRvs = map phi_inverse (randomRs (0.0, 1.0) (mkStdGen s))
            uniformRvs = randomRs (0, yLevels) (mkStdGen s)
            numberOfWalls = round (min (meanNumberOfWalls + meanNumberOfWalls * (head normalRvs)) minNumberOfWalls)
            platformLenghts = map (\x -> x * meanWallLength + wallBase) (take numberOfWalls normalRvs)
            platformYOrigins = map (\x -> ( (fromIntegral(x) / fromIntegral(yLevels)) * (maxWallY - minWallY) + minWallY)) (take numberOfWalls uniformRvs)
            platformXOrigins = scanl (+) xOrigin (map (\x -> baseOriginOffset + meanOriginOffset * x) (take numberOfWalls normalRvs))

generateRandomVerticalWalls :: Int -> Double -> [GameObject]
generateRandomVerticalWalls s xOrigin = zipWith3 makeVerticalWall platformXOrigins platformYOrigins platformLenghts
        where
            normalRvs = map phi_inverse (randomRs (0.0, 1.0) (mkStdGen s))
            numberOfWalls = round (min (head normalRvs) 1)
            platformLenghts = map (\x -> x * meanWallLength + wallBase) (take numberOfWalls normalRvs)
            platformYOrigins =  take numberOfWalls [0.0, 0.0 ..]
            platformXOrigins = scanl (+) xOrigin (map (\x -> baseOriginOffset + meanOriginOffset * x) (take numberOfWalls normalRvs))

generateRandomDoors :: Int -> Double -> [GameObject]
generateRandomDoors s xOrigin = concat (zipWith makeDoorButtonPair platformXOrigins platformYOrigins)
        where
            normalRvs = map phi_inverse (randomRs (0.0, 1.0) (mkStdGen s))
            numberOfDoors = 1 * (fromEnum ((head normalRvs) > 1.2)) -- 11 % chance to get a door 
            platformYOrigins =  take numberOfDoors [-1.0, -1.0 ..]
            platformXOrigins = scanl (+) xOrigin (map (\x -> baseOriginOffset + meanOriginOffset * x) (take numberOfDoors normalRvs))


makeDoorButtonPair :: Double -> Double -> [GameObject]
makeDoorButtonPair x y = [makeDoor x, makeButton x y]

makeDoor :: Double -> GameObject
makeDoor x = GameObject bounds (Door x)
    where
        doorOffset = 10.0
        bounds = Bounds p1 p2 p3 p4
        p1 = Point (x + doorOffset) screenHeight
        p2 = Point (x + doorOffset + 1.5) screenHeight
        p3 = Point (x + doorOffset + 1.5) ( - screenHeight)
        p4 = Point (x + doorOffset) ( - screenHeight)

makeButton :: Double -> Double -> GameObject
makeButton x y = GameObject bounds (Button x)
    where
        bounds = Bounds p1 p2 p3 p4
        p1 = Point x y
        p2 = Point (x + 1) y
        p3 = Point (x + 1) (y - 1)
        p4 = Point x (y - 1)

makeSpike :: Double -> Double -> GameObject
makeSpike x y = GameObject bounds Spikes
    where
        bounds = Bounds p1 p2 p3 p4
        p1 = Point x y
        p2 = Point (x + 0.5) y
        p3 = Point (x + 0.5) (y - 0.5)
        p4 = Point x (y - 0.5)

makeCoin :: Double -> Double -> GameObject
makeCoin x y = GameObject bounds Coin
    where
        bounds = Bounds p1 p2 p3 p4
        p1 = Point x y
        p2 = Point (x + 0.25) y
        p3 = Point (x + 0.25) (y - 0.25)
        p4 = Point x (y - 0.25)


generateRandomSpikes :: Int -> Double -> [GameObject]
generateRandomSpikes s xOrigin = zipWith makeSpike spikeXOrigins spikeYOrigins
        where
            uniformRvs = randomRs (0, yLevels - 1) (mkStdGen s)
            numberOfspikes = meanNumberOfSpikes
            xPositions = map (wallBase * ) [0, 1 .. ]
            spikeYOrigins = map (\x -> ( (fromIntegral(x) / fromIntegral(yLevels)) * (maxWallY - minWallY) + minWallY) - 0.5) (take numberOfspikes uniformRvs)
            spikeXOrigins = map (xOrigin +)  xPositions


-- TODO insert this into generateRandomWalls
isFeasible :: [GameObject] -> Bool
isFeasible gs = not (null gs) && G.path (makeGraph gs) 1 (length gs)


-- Generate graph with walls as vertices and paths inbetween as edges
makeGraph :: [GameObject] -> G.Graph
makeGraph walls = G.buildG b edges
    where
        b = (1, (length walls))
        inBounds = zip [1..] (map getIncomingTrapezoids walls)
        outBounds = zip [1..] (map getOutcomingTrapezoids walls)
        allBounds = unzip [(inB, outB) | inB <- inBounds, outB <- outBounds]
        edges = catMaybes $ zipWith getEdges  (fst allBounds)  (snd allBounds)

        getEdges :: (Int, (Bounds, Bounds)) -> (Int, (Bounds, Bounds)) -> Maybe (Int, Int)
        getEdges (i, (inBup, inBdown)) (j, (outBup, outBdown)) | edgeExists inBup inBdown outBup outBdown = Just (i, j)
                                                               | otherwise = Nothing
                                                               
edgeExists :: Bounds -> Bounds -> Bounds -> Bounds -> Bool
edgeExists inUp inDown outUp outDown = fromUpToDown || fromDownToUp
    where
    fromUpToDown = not $ isNothing (intersectTrapBounds inUp outDown)
    fromDownToUp = not $ isNothing (intersectTrapBounds inDown outUp)


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


-- Get horizaontal speed based on player current position
getHorizontalSpeed :: Double -> Double
getHorizontalSpeed x = horizontalSpeed
    where
        horizontalSpeed = x / timeFromX(horizontalAcceleration / 2, initialSpeed, x)
        timeFromX (a, b, c) = t1
            where
                e = -b * (2 * a)
                d = b * b - 4 * a * (-c)
                t1 = e + sqrt (d / (2 * a))

-- Obtain incoming trapezoids for wall
getIncomingTrapezoids :: GameObject -> (Bounds, Bounds)
getIncomingTrapezoids (GameObject bound _) = (getIncomingUpperBound (topLeft bound) (topRight bound), getIncomingLowerBound (bottomLeft bound) (bottomRight bound))
    where
        getIncomingUpperBound leftPoint rightPoint = Bounds (getIncomingUpperBoundPoint leftPoint) (getIncomingUpperBoundPoint rightPoint) rightPoint leftPoint
        getIncomingUpperBoundPoint (Point x y) = (Point (calculateIncomingBoundXPoint x (screenHeight - y)) screenHeight)
        getIncomingLowerBound leftPoint rightPoint = Bounds (getIncomingLowerBoundPoint leftPoint) (getIncomingLowerBoundPoint rightPoint) rightPoint leftPoint
        getIncomingLowerBoundPoint (Point x y) = (Point (calculateIncomingBoundXPoint x y) 0)
        calculateIncomingBoundXPoint x dY = x - getHorizontalSpeed (x - time) * time - 0.5 * horizontalAcceleration * (time ^ 2)
            where
                time = dY / verticalSpeed

-- Obtain outcoming trapezoids for wall
getOutcomingTrapezoids :: GameObject -> (Bounds, Bounds)
getOutcomingTrapezoids (GameObject bound _) = (getOutcomingUpperBound (topLeft bound) (topRight bound),  getOutcomingLowerBound (bottomLeft bound) (bottomRight bound))
        where
            getOutcomingUpperBound leftPoint rightPoint = Bounds (getOutcomingUpperBoundPoint leftPoint) (getOutcomingUpperBoundPoint rightPoint) rightPoint leftPoint
            getOutcomingUpperBoundPoint (Point x y) = (Point (calculateOutcomingBoundXPoint x (screenHeight - y)) screenHeight)
            getOutcomingLowerBound leftPoint rightPoint = Bounds leftPoint rightPoint (getOutcomingLowerBoundPoint rightPoint) (getOutcomingLowerBoundPoint leftPoint)
            getOutcomingLowerBoundPoint (Point x y) = (Point (calculateOutcomingBoundXPoint x y) 0)
            calculateOutcomingBoundXPoint x dY = x + getHorizontalSpeed x * time + 0.5 * horizontalAcceleration * (time ^ 2)
                where
                    time = dY / verticalSpeed


module HaskRunner.Physics where

import HaskRunner.Core

-- Adjust velocity based on gravity and bounds
adjustVelocity :: Double -> [Bounds] -> Bounds -> (Velocity, Velocity) -> (Velocity, Velocity)
adjustVelocity gravity objects player (hor, ver) = (newHor, newVert)
  where
    getCollisions = collisions player objects (hor, ver)
    newGravity = gravityEffect getCollisions gravity
    (h, v) = adjustForCollisions getCollisions (hor, ver)
    newHor = h
    newVert = newGravity + v

gravityEffect :: [CollisionType] ->  Double -> Double
gravityEffect collisions gravity
  | gravity < 0 && CUp `elem` collisions = 0
  | gravity > 0 && CDown `elem` collisions = 0
  | otherwise = gravity

-- adjust velocity according to collision types
adjustForCollisions :: [CollisionType]
  -> (Velocity, Velocity)
  -> (Velocity, Velocity)
adjustForCollisions [] velocity = velocity
adjustForCollisions (collision:collisions) velocity
  = adjustSingleCollision collision velocity `join`
    adjustForCollisions collisions velocity

adjustSingleCollision :: CollisionType
  -> (Velocity, Velocity)
  -> (Velocity, Velocity)
adjustSingleCollision collision (hor, ver) = adjust collision (hor, ver)
  where
    adjust CUp (h, v) = (h, if v < 0 then 0 else v)
    adjust CDown (h, v) = (h, if v > 0 then 0 else v)
    adjust CLeft (h, v) = (0, v)
    adjust CRight (h, v) = (0, v)

-- if one of the velocities is 0, set result to 0,
join :: (Velocity, Velocity) -> (Velocity, Velocity) -> (Velocity, Velocity)
join (h1, v1) (h2, v2) = (h1 `vAnd` h2, v1 `vAnd` v2)
  where
    vAnd 0 _ = 0
    vAnd _ 0 = 0
    vAnd a _ = a

-- get all collisions that occured
collisions :: Bounds
  -> [Bounds]
  -> (Velocity, Velocity)
  -> [CollisionType]
collisions object otherObjects (h, v) = collisionTypes
  where
    collidedObjects = collisionHappened otherObjects object
    collisionTypes = collisionType oldObject object collidedObjects
    -- approximate old position
    oldObject = moveBounds object ((-h), (-v))

-- check if collision occured, and return all objects for which it happened
collisionHappened :: [Bounds] -> Bounds -> [Bounds]
collisionHappened objects object = collidingObjects
  where
    collidingObjects = filter (collided object) objects

-- AABB collision
collided :: Bounds -> Bounds -> Bool
collided o1 o2 = collidedX && collidedY
  where
    collidedX
      = x (bottomRight o1) >= x (topLeft o2) &&
        x (bottomRight o2) >= x (topLeft o1)
    collidedY
      = y (bottomRight o1) <= y (topLeft o2) &&
        y (bottomRight o2) <= y (topLeft o1)
    x (Point c1 _) = c1
    y (Point _ c2) = c2

-- return all collision directions which occured with the object
collisionType :: Bounds -> Bounds -> [Bounds] -> [CollisionType]
collisionType _ _ [] = []
collisionType oldObj obj (bound:bounds) = collisions ++
    (collisionType oldObj obj bounds)
  where
    collisions
      | collidedFromLeft = [CLeft]
      | collidedFromRight = [CRight]
      | collidedFromTop = [CUp]
      | collidedFromBottom = [CDown]
      | otherwise = []
    collidedFromLeft
      = right oldObj < left bound && right obj >= left bound
    collidedFromRight
      = left oldObj >= right bound && left obj < right bound
    collidedFromTop
      = bottom oldObj > top bound && bottom obj <= top bound
      || bottom oldObj <= top bound && bottom obj <= top bound && top obj >= top bound
    collidedFromBottom
      = top oldObj < bottom bound && top obj >= bottom bound
      || top oldObj >= bottom bound && top obj >= bottom bound && bottom obj <= bottom bound
    left = x . topLeft
    right = x . bottomRight
    top = y . topLeft
    bottom = y . bottomRight
    x (Point c1 _) = c1
    y (Point _ c2) = c2

-- CollisionType shows from which side object crossed other object:
-- It is calculated relative to nonmoving object
data CollisionType = CUp | CDown | CLeft | CRight
  deriving (Eq)

adjustGravity :: Bool-> Double -> Double
adjustGravity True base  = base
adjustGravity False base = -base


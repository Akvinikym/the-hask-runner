module HaskRunner.Physics where

import HaskRunner.Core

-- updates the velocities based on whether or not collision happened
collision :: Bounds -> [Bounds] -> (Velocity, Velocity) -> (Velocity, Velocity)
collision object otherObjects (h, v) = (newHor, newVert)
  where
    hasCollided = collisionHappened otherObjects object
    (newHor, newVert) = adjustVelocity (collisionType oldObject object hasCollided) (h, v)
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
      = x (bottomRight o1) >= x (topLeft o2) && x (bottomRight o2) >= x (topLeft o1)
    collidedY
      = y (bottomRight o1) <= y (topLeft o2) && y (bottomRight o2) <= y (topLeft o1)
    x (Point c1 _) = c1
    y (Point _ c2) = c2

-- return all collision directions which occured with the object
collisionType :: Bounds -> Bounds -> [Bounds] -> [CollisionType]
collisionType _ _ [] = []
collisionType oldObj obj (bound:bounds) = collisions ++ (collisionType oldObj obj bounds)
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
    collidedFromBottom
      = top oldObj < bottom bound && top obj >= bottom bound
    left = x . topLeft
    right = x . bottomRight
    top = y . topLeft
    bottom = y . bottomRight
    x (Point c1 _) = c1
    y (Point _ c2) = c2

-- CollisionType shows from which side object crossed other object:
-- It is calculated relative to nonmoving object
data CollisionType = CUp | CDown | CLeft | CRight

-- adjust velocity according to collision types
adjustVelocity :: [CollisionType] -> (Velocity, Velocity) -> (Velocity, Velocity)
adjustVelocity [] velocity = velocity
adjustVelocity (collision:collisions) (hor, ver) = adjust collision
  where
    adjust CUp = (hor, 0)
    adjust CDown = (hor, 0)
    adjust CLeft = (0, ver)
    adjust CRight = (0, ver)

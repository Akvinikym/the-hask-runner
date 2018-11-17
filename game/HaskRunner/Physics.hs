module HaskRunner.Physics where

import HaskRunner.Core

-- Adjust velocity based on gravity and bounds
-- moves object based on speed
-- then sees if it is possible to move in that position
adjustVelocity :: Double
               -> Double
               -> Double
               -> [Bounds]
               -> Bounds
               -> (Velocity, Velocity)
               -> (Velocity, Velocity)
adjustVelocity worldVel dt gravity objects player (hor, ver) = (adjustHor hCollisions, adjustVert vCollisions)
  where
    newHor = if hor == 0 then 0 + horizontalAcceleration else 0
    newVert = ver + gravity * dt
    hCollisions = hcollisions player objects newHor
    vCollisions = vcollisions player objects newVert
    adjustVert True = if hCollisions then newVert else 0
    adjustVert _ = newVert
    adjustHor True = -worldVel
    adjustHor _ = 0

-- detect if any horizontal motion causes collision
hcollisions :: Bounds -> [Bounds] -> Double -> Bool
hcollisions object objects vel = any (collided newObj) objects
  where
    newObj = moveBounds object (vel, 0)

-- detect if any vertical motion causes collision
vcollisions :: Bounds -> [Bounds] -> Double -> Bool
vcollisions object objects vel = any (collided newObj) objects
  where
    newObj = moveBounds object (0, vel)

-- AABB collision
collided :: Bounds -> Bounds -> Bool
collided o1 o2 = collidedX && collidedY
  where
    collidedX
      = x (bottomRight o1) > x (topLeft o2) &&
        x (bottomRight o2) > x (topLeft o1)
    collidedY
      = y (bottomRight o1) < y (topLeft o2) &&
        y (bottomRight o2) < y (topLeft o1)
    x (Point c1 _) = c1
    y (Point _ c2) = c2

-- reverse gravity on false
adjustGravity :: Bool-> Double -> Double
adjustGravity True base  = base
adjustGravity False base = -base


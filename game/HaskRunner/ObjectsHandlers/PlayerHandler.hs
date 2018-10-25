module HaskRunner.ObjectsHandlers.PlayerHandler where

import HaskRunner.Core

-- | Player's objects supply functions

collision :: Bounds -> Level -> (Velocity, Velocity) -> (Velocity, Velocity)
collision position level (h, v) = (newHor, newVert)
  where
    newHor
      | horCollision = 0
      | otherwise = h
    newVert
      | vertCollision = 0
      | otherwise = v
    horCollision = False
    vertCollision = any collides (map bounds (levelMap level))
    y (Point _c1 c2) = c2
    x (Point c1 _c2) = c1
    collides bound
      = y (bottomLeft position) <= y (topLeft bound) && y (bottomRight position) <= y (topRight bound)
       &&
       ((x (bottomLeft position) > x (topLeft bound) && x (bottomLeft position) < x (bottomRight bound))
      || (x (bottomRight position) > x (topLeft bound) && x (bottomRight position) < x (bottomRight bound)))

-- move the player according to his velocity and gravity
movePlayer :: Level -> Level
movePlayer level
  = level { player = Player newPosition newHorVelocity newVertVelocity }
  where
    newPosition = moveBounds bounds (hor, vert)
    bounds = pbounds (player level)
    hor = pHorVelocity (player level)
    vert = pVertVelocity (player level)
    (v, h) = collision newPosition level (hor, vert)
    newHorVelocity = v
    newVertVelocity = h


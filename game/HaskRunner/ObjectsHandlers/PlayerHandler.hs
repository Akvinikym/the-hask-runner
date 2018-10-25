module HaskRunner.ObjectsHandlers.PlayerHandler where

import HaskRunner.Core
import HaskRunner.Physics

-- | Player's objects supply functions

-- move the player according to his velocity and gravity
movePlayer :: Level -> Level
movePlayer level
  = level { player = Player newPosition newHorVelocity newVertVelocity }
  where
    newPosition = moveBounds (pbounds (player level)) (hor, vert)
    hor = pHorVelocity (player level)
    vert = pVertVelocity (player level)
    (v, h) = collision newPosition (map bounds (levelMap level)) (hor, vert)
    newHorVelocity = v
    newVertVelocity = h


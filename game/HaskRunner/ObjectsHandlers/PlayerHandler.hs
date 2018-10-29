module HaskRunner.ObjectsHandlers.PlayerHandler where

import HaskRunner.Core
import HaskRunner.Physics

-- | Player's objects supply functions

-- move the player according to his velocity and gravity
movePlayer :: Level -> Level
movePlayer level
  = level { player = Player newPosition h v, acceleration = newAcceleration v}
  where
    -- TODO: move speed adjustments to physics
    newPosition = moveBounds (pbounds (player level)) (h, v)
    hor = pHorVelocity (player level)
    vert = (acceleration level) + pVertVelocity (player level)
    (h, v)
      = collision
        (pbounds (player level))
        (map bounds (levelMap level ++ (edges level))) (hor, vert)
    newAcceleration 0 = 0
    newAcceleration _ = acceleration level

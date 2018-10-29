module HaskRunner.ObjectsHandlers.PlayerHandler where

import HaskRunner.Core
import HaskRunner.Physics

-- | Player's objects supply functions

-- move the player according to his velocity and gravity
movePlayer :: Level -> Level
movePlayer level
  = level { player = Player newPosition h v}
  where
    -- TODO: move speed adjustments to physics
    newPosition = moveBounds (pbounds (player level)) (h, v)
    hor = pHorVelocity (player level)
    vert = (adjustGravity (gravityIsDown level) baseGravity) + pVertVelocity (player level)
    (h, v)
      = collision
        (pbounds (player level))
        (map bounds (levelMap level ++ (edges level))) (hor, vert)

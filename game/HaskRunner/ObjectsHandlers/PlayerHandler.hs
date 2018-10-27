module HaskRunner.ObjectsHandlers.PlayerHandler where

import HaskRunner.Core
import HaskRunner.Physics

-- | Player's objects supply functions

-- move the player according to his velocity and gravity
movePlayer :: Level -> Level
movePlayer level
  = level { player = Player newPosition h v }
  where
    newPosition = moveBounds (pbounds (player level)) (h, v)
    hor = pHorVelocity (player level)
    vert = (acceleration level) + pVertVelocity (player level)
    (h, v) = collision (pbounds (player level))(map bounds (levelMap level)) (hor, vert)


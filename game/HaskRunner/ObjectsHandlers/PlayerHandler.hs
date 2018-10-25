module HaskRunner.ObjectsHandlers.PlayerHandler where

import HaskRunner.Core

-- | Player's objects supply functions

-- move the player up or down if he is not staying on platforms
movePlayer :: Level -> Level
movePlayer level = level { player = Player newBounds }
  where
    Level (Player bounds) _ _ gravityIsDown _ vertVelocity _ _ = level
    newBounds
        | gravityIsDown = moveBounds bounds (0, -1 * vertVelocity)
        | otherwise     = moveBounds bounds (0, vertVelocity)

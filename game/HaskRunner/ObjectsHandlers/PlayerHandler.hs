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

-- find the player's center coordinate assuming it's a rectangle
playerCenter :: Player -> Point
playerCenter (Player (Bounds (Point x1 y1) _ (Point x2 y2) _))
    = Point ((x1 + x2) / 2) ((y1 + y2) / 2)

playerWidthHeight :: Player -> (Double, Double)
playerWidthHeight (Player (Bounds (Point x1 y1) _ (Point x2 y2) _))
    = (x2 - x1, y2 - y1)

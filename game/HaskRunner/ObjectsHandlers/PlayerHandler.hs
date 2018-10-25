module HaskRunner.ObjectsHandlers.PlayerHandler where

import HaskRunner.Core

-- | Player's objects supply functions

-- compute players location according to its velocity
newPlayer :: Player -> Player
newPlayer (Player bounds hor vert) = Player newBounds hor vert
  where
    newBounds = moveBounds bounds (hor, vert)

-- move the player according to his velocity and gravity
movePlayer :: Level -> Level
movePlayer level
  = level { player = (newPlayer (player level))}

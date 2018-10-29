module HaskRunner.ObjectsHandlers.PlayerHandler where

import HaskRunner.Core
import HaskRunner.Physics

-- | Player's objects supply functions

-- move the player according to his velocity and gravity
movePlayer :: Level -> Level
movePlayer level
  = level { player = Player newPosition h v }
  where
    -- TODO: move speed adjustments to physics
    newPosition = moveBounds (pbounds (player level)) (h, v)
    hor = pHorVelocity (player level)
    vert = (acceleration level) + pVertVelocity (player level)
    (h, v) = collision 
      (pbounds (player level)) 
      (map bounds (levelMap level)) 
      (hor, vert)

-- find out, if the player dies, collided with some obstacle
playerDied :: Level -> Bool
playerDied (Level (Player pBounds _ _) objects _ _ _ _ _ _ _)
    = any deadCollision onScreenObjects
  where
    onScreenObjects = filter onScreen objects
    deadCollision object 
        = collided pBounds (bounds object) && deadObject object

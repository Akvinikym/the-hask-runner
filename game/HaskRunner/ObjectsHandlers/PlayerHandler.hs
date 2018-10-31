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
      (map bounds objectsOnScreen)
      (hor, vert)
    objectsOnScreen = takeWhile (onScreen level) 
      (dropWhile (not . (onScreen level)) (levelMap level))

-- find out, if the player dies, collided with some obstacle
playerDied :: Level -> Bool
playerDied level = any deadCollision objectsOnScreen
  where
    (Level (Player pBounds _ _) objects _ _ _ _ _ _ _ _) = level
    objectsOnScreen 
        = takeWhile (onScreen level) 
          (dropWhile (not . (onScreen level)) (levelMap level))
    deadCollision object 
        = collided pBounds (bounds object) && deadObject object

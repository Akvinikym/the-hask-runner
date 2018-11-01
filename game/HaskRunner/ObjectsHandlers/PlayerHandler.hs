module HaskRunner.ObjectsHandlers.PlayerHandler where

import HaskRunner.Core
import HaskRunner.Physics

-- | Player's objects supply functions

-- move the player according to his velocity and gravity
movePlayer :: Level -> Level
movePlayer level
  = level { player = (Player newPosition h v)}
  where
    newPosition = moveBounds playerBounds (h, v)
    playerBounds = pbounds (player level)
    currentGravity = adjustGravity (gravityIsDown level) baseGravity
    levelObjects = map bounds (levelMap level ++ (edges level))
    worldVel = horVelocity level
    hor = pHorVelocity (player level)
    vert = pVertVelocity (player level)
    (h, v) = adjustVelocity
      worldVel
      currentGravity
      (map bounds objectsOnScreen)
      (pbounds (player level))
      (hor, vert)
    objectsOnScreen = takeWhile (onScreen level)
      (dropWhile (not . (onScreen level)) (levelMap level)) <> edges level

-- find out, if the player dies, collided with some obstacle
playerDied :: Level -> Bool
playerDied level = any deadCollision objectsOnScreen
  where
    (Level (Player pBounds _ _) objects _ _ _ _ _ _ _) = level
    objectsOnScreen
        = takeWhile (onScreen level)
          (dropWhile (not . (onScreen level)) (levelMap level)) <> edges level
    deadCollision object
        = collided pBounds (bounds object) && deadObject object

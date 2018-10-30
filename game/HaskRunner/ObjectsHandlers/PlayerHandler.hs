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
    hor = pHorVelocity (player level)
    vert = pVertVelocity (player level)
    (h, v) = adjustVelocity currentGravity levelObjects playerBounds (hor, vert)

-- find out, if the player dies, collided with some obstacle
playerDied :: Level -> Bool
playerDied (Level (Player pBounds _ _) objects edges _ _ _ _ _)
    = any deadCollision onScreenObjects
  where
    onScreenObjects = filter onScreen (objects ++ edges)
    deadCollision object
        = collided pBounds (bounds object) && deadObject object

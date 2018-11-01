module HaskRunner.ObjectsHandlers.PlayerHandler where

import HaskRunner.Core
import HaskRunner.Physics
import Data.List

-- | Player's objects supply functions

absolutePosition :: Double -> Bounds -> Bounds
absolutePosition dist bounds
  = moveBounds bounds (dist, 0)

-- move the player according to his velocity and gravity
movePlayer :: Level -> Level
movePlayer level
  = level { player = (Player newPosition h v)}
  where
    newPosition = moveBounds playerBounds (h, v)
    playerBounds = pbounds (player level)
    currentAbsPos = absolutePosition (levelPos level) playerBounds
    currentGravity = adjustGravity (gravityIsDown level) baseGravity
    worldVel = horVelocity level
    hor = pHorVelocity (player level)
    vert = pVertVelocity (player level)
    (h, v) = adjustVelocity
      worldVel
      currentGravity
      (map bounds objectsOnScreen)
      currentAbsPos
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
        = (collided absPos (bounds object) || collided pBounds (bounds object))
          && deadObject object
    playerBounds = pbounds (player level)
    currentAbsPos = absolutePosition (levelPos level) playerBounds
    absPos = moveBounds currentAbsPos (horizontalAcceleration, pVertVelocity (player level))

-- add points if player has picked up any coins and remove these coins from the game
checkCoins :: Level -> Level
checkCoins level = level {lilcoins = coins, levelMap = lmap}
  where
    lmap = deleteBy coinCollision undefined (levelMap level)
    coins = if any (coinCollision undefined) objectsOnScreen then (lilcoins level) + 1 else lilcoins level
    coinCollision  _ object
        = collided absPos (bounds object)
          && (objectType object) == Coin
    objectsOnScreen
        = takeWhile (onScreen level)
          (dropWhile (not . (onScreen level)) (levelMap level))
    playerBounds = pbounds (player level)
    currentAbsPos = absolutePosition (levelPos level) playerBounds
    absPos = moveBounds currentAbsPos (horizontalAcceleration, pVertVelocity (player level))

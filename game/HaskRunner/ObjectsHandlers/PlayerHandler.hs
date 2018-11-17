module HaskRunner.ObjectsHandlers.PlayerHandler where

import HaskRunner.Core
import HaskRunner.Physics
import Data.List

-- | Player's objects supply functions

absolutePosition :: Double -> Bounds -> Bounds
absolutePosition dist bounds
  = moveBounds bounds (dist, 0)

-- move the players according to his velocity and gravity
movePlayer :: Double -> Player -> Level -> Level
movePlayer dt player level
    | player == (player1 level) = level {player1 = newPlayer}
    | otherwise                 = level {player2 = newPlayer}
  where
    newPlayer = Player newPosition h v (gravityIsDown player) (lilcoins player)

    newPosition = moveBounds playerBounds (h, v)
    playerBounds = pbounds player
    currentAbsPos = absolutePosition (levelPos level) playerBounds
    currentGravity = adjustGravity (gravityIsDown player) baseGravity
    worldVel = horVelocity level
    hor = pHorVelocity player
    vert = pVertVelocity player
    (h, v) = adjustVelocity
      worldVel
      dt
      currentGravity
      (map bounds objectsOnScreen)
      currentAbsPos
      (hor, vert)
    objectsOnScreen = takeWhile (onScreen level)
      (dropWhile (not . (onScreen level)) (levelMap level)) <> edges level

-- find out, if the player dies, collided with some obstacle
playerDied :: Level -> Player -> Bool
playerDied level player = any deadCollision objectsOnScreen
  where
    (Level _ _ objects _ _ _ _) = level
    objectsOnScreen
        = takeWhile (onScreen level)
          (dropWhile (not . (onScreen level)) (levelMap level)) <> edges level
    deadCollision object
        = (collided absPos (bounds object) || collided playerBounds (bounds object))
          && deadObject object
    playerBounds = pbounds player
    currentAbsPos = absolutePosition (levelPos level) playerBounds
    absPos = moveBounds currentAbsPos (horizontalAcceleration, pVertVelocity player)

-- add points if player has picked up any coins and remove these coins from the game
checkCoins :: Player -> Level -> Level
checkCoins player level
    | player == player1 level
        = level { player1 = __player1 {lilcoins = coins}, levelMap = lmap}
    | otherwise
        = level { player2 = __player2 {lilcoins = coins}, levelMap = lmap}
  where
    __player1 = player1 level
    __player2 = player2 level

    lmap = deleteBy coinCollision undefined (levelMap level)
    coins = if any (coinCollision undefined) objectsOnScreen then (lilcoins player) + 1 else lilcoins player
    coinCollision  _ object
        = collided absPos (bounds object)
          && (objectType object) == Coin
    objectsOnScreen
        = takeWhile (onScreen level)
          (dropWhile (not . (onScreen level)) (levelMap level))
    playerBounds = pbounds player
    currentAbsPos = absolutePosition (levelPos level) playerBounds
    absPos = moveBounds currentAbsPos (horizontalAcceleration, pVertVelocity player)

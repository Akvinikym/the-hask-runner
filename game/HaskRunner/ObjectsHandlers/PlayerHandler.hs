module HaskRunner.ObjectsHandlers.PlayerHandler where

import HaskRunner.Core
import HaskRunner.Physics
import Data.List
import Data.Maybe

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
    newPlayer = Player 
        (name player)
        newPosition 
        h 
        v 
        (gravityIsDown player) 
        (lilcoins player) 
        (distance player)
        (isDead player)

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
      (map bounds (objectOnScreenWithoutCoins <> edges level))
      currentAbsPos
      (hor, vert)

      where
        objectOnScreenWithoutCoins
            = filter ((/= Coin) . objectType) (objectsOnScreen level)

-- find out, if the player dies, collided with some obstacle
playerDied :: Level -> Player -> Bool
playerDied level player = any deadCollision (objectsOnScreen level <> edges level)
  where
    (Level _ _ objects _ _ _ _ _) = level
    deadCollision object
        = (collided absPos (bounds object) || collided playerBounds (bounds object))
          && deadObject object
    playerBounds = pbounds player
    currentAbsPos = absolutePosition (levelPos level) playerBounds
    absPos = moveBounds currentAbsPos (horizontalAcceleration, pVertVelocity player)

-- add coin if player has picked any and remove this coin from the game
checkCoins :: Player -> Level -> Level
checkCoins player level
    | player == __player1
        = level { player1 = __player1 {lilcoins = coins}, levelMap = lmap}
    | otherwise
        = level { player2 = __player2 {lilcoins = coins}, levelMap = lmap}
  where
    __player1 = player1 level
    __player2 = player2 level

    lmap = deleteBy coinCollision undefined (levelMap level)
    coins = if any (coinCollision undefined) (objectsOnScreen level) then 
            (lilcoins player) + 1 
        else 
            lilcoins player
    coinCollision _ object
        = collided absPos (bounds object)
          && (objectType object) == Coin
    playerBounds = pbounds player
    currentAbsPos = absolutePosition (levelPos level) playerBounds
    absPos 
        = moveBounds currentAbsPos (horizontalAcceleration, pVertVelocity player)

-- add the current level distance to players' ones, if they're not dead
checkDistances :: Level -> Level
checkDistances level = level 
    { player1 = __player1 {distance = newDistance1}
    , player2 = __player2 {distance = newDistance2}
    }
  where
    __player1 = player1 level
    __player2 = player2 level

    newDistance1 = if (not (isDead __player1))
        then floor (levelPos level)
        else distance __player1
    newDistance2 = if (not (isDead __player2))
        then floor (levelPos level)
        else distance __player2

-- kill the player
killPlayer :: Player -> Player
killPlayer player = player { isDead = True }


-- openDoor if player has pressed button  and remove this button from the game
checkDoors :: Level -> Level
checkDoors level = level {levelMap = lmap}
    where
      lmap = deleteBy doorCheck undefined (levelMap level)
      doorCheck _ (GameObject b (Door d)) = elem d (doorsOpened level)
      doorCheck _ _ = False

-- openDoor if player has pressed button  and remove this button from the game
checkButtons :: Player -> Level -> Level
checkButtons player level = level { doorsOpened = __openedDoors, levelMap = lmap}
    where
      lmap = deleteBy buttonCollision undefined (levelMap level)
      buttonsCollided = filter (buttonCollision undefined) objectsOnScreen
      __openedDoors = (doorsOpened level) ++ (catMaybes (map getButtonId buttonsCollided))
      getButtonId :: GameObject -> Maybe Double
      getButtonId (GameObject b (Button id)) =  Just id
      getButtonId _ = Nothing
      buttonCollision :: a -> GameObject -> Bool
      buttonCollision _ (GameObject b (Button _)) = collided absPos b
      buttonCollision _ _ = False
      objectsOnScreen
          = takeWhile (onScreen level)
            (dropWhile (not . (onScreen level)) (levelMap level))
      playerBounds = pbounds player
      currentAbsPos = absolutePosition (levelPos level) playerBounds
      absPos = moveBounds currentAbsPos (horizontalAcceleration, pVertVelocity player)

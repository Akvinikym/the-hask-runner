module HaskRunner.ObjectsHandlers.LevelHandler where

-- | Level's supply functions

import HaskRunner.Core
import HaskRunner.ObjectsHandlers.PlayerHandler

-- increase level's velocity based on time from the last frame
increaseLevelVelocity :: Double -> Level -> Level
increaseLevelVelocity dt level = level {
    horVelocity = newVelocity,
    levelPos = newVelocity + (levelPos level)
  }
  where
    newVelocity = (horVelocity level) + dt * horizontalAcceleration

playersDeaths :: Level -> Level
playersDeaths level
    | isDead __player1 && isDead __player2 = level { state = Dead }
    | otherwise
        = level 
        { player1 = playerDeath __player1 level
        , player2 = playerDeath __player2 level
        }
  where
    __player1 = player1 level
    __player2 = player2 level

-- check, if player died; if so, update him accordingly
playerDeath :: Player -> Level -> Player
playerDeath player level
    | playerDied level player = killPlayer player
    | otherwise               = player

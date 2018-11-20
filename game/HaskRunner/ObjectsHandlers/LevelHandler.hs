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
    | otherwise                            = level
  where
    __player1 = player1 level
    __player2 = player2 level

-- check, if player died; if so, update him accordingly
checkPlayerDeath :: Player -> Level -> Level
checkPlayerDeath player level
    | playerDied level player =
        if (player == __player1) then
            level { player1 = killPlayer player }
        else
            level { player2 = killPlayer player }
    | otherwise               = level
  where
    __player1 = player1 level
    __player2 = player2 level

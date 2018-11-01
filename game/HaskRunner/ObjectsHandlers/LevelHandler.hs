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

-- check, if player died; if so, update the level accordingly
playerDeath :: Level -> Level
playerDeath level
    | playerDied level = level { isFinished = True }
    | otherwise        = level

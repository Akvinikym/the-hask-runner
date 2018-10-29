module HaskRunner.ObjectsHandlers.GameObjectsHandler where

-- | Contains functions for handling generated game objects

import HaskRunner.Core

-- move objects to left according to world's velocity
moveObjects :: Level -> Level
moveObjects level = level
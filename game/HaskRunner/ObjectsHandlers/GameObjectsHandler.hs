module HaskRunner.ObjectsHandlers.GameObjectsHandler where

-- | Contains functions for handling generated game objects

import HaskRunner.Core

-- move objects to left according to world's velocity
-- moveObjects :: Level -> Level
-- moveObjects level 
--     = level { levelMap = map moveObject (takeWhile onScreen objects) }
--   where
--     objects = levelMap level
--     hVelocity = horVelocity level
--     moveObject obj 
--       = obj { bounds = moveBounds (bounds obj) ((- hVelocity), 0) }

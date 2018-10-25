module HaskRunner.Graphics.Drawers where

import CodeWorld
import HaskRunner.Core

-- | All kinds of drawers are located here

-- draw player and all objects currently on the screen
drawLevel :: Level -> Picture
drawLevel level = drawPlayer (player level)

-- draw player
drawPlayer :: Player -> Picture
drawPlayer (Player (Bounds (Point x1 y1) _ (Point x2 y2) _)) 
    = coloured green playerRect
  where
    width  = x2 - x1
    height = y2 - y1
    playerRect = solidRectangle width height

module HaskRunner.Graphics.Drawers where

import CodeWorld
import HaskRunner.Core
import HaskRunner.ObjectsHandlers.PlayerHandler

-- | All kinds of drawers are located here

-- draw player and all objects currently on the screen
drawLevel :: Level -> Picture
drawLevel level = drawPlayer (player level)

-- draw player
drawPlayer :: Player -> Picture
drawPlayer player = coloured green playerRect
  where
    (width, height) = playerWidthHeight player
    (Point centerX centerY) = playerCenter player
    playerRect = translated centerX centerY (solidRectangle width height)

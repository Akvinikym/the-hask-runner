module HaskRunner.Graphics.Drawers where

import CodeWorld
import HaskRunner.Core

-- | All kinds of drawers are located here

-- draw player and all objects currently on the screen
drawLevel :: Level -> Picture
drawLevel level
    | isFinished level = drawGameOverScreen
    | otherwise        = (drawPlayer (player level))
      <> foldr ((<>) . drawObject) blank objectsOnScreen
      <> foldr ((<>) . drawObject) blank (edges level)
  where
    objectsOnScreen = filter onScreen (levelMap level)

-- draw player
drawPlayer :: Player -> Picture
drawPlayer player = drawRectangularObject (pbounds player) green

-- draw a game object
drawObject :: GameObject -> Picture
drawObject (GameObject bounds Platform)
    = drawRectangularObject bounds brown
drawObject (GameObject bounds Wall)
    = drawRectangularObject bounds (darker 0.5 brown)
drawObject (GameObject bounds Spikes)
    = drawTriangularObject bounds red
drawObject (GameObject bounds Coin)
    = drawCircularObject bounds yellow

drawRectangularObject :: Bounds -> Colour -> Picture
drawRectangularObject bounds colour
    = coloured colour rect
      where
        (width, height) = boundsWidthHeight bounds
        (Point centerX centerY) = boundsCenter bounds
        rect = translated centerX centerY (solidRectangle width height)

drawTriangularObject :: Bounds -> Colour -> Picture
drawTriangularObject bounds colour
    = coloured colour triangle
      where
        (Bounds (Point x1 y1) _ (Point x3 y3) (Point x4 y4)) = bounds
        (Point centerX centerY) = boundsCenter bounds
        triangle = translated centerX centerY
          (solidPolygon [((x1 + x3) / 2, y1), (x3, y3), (x4, y4)])

drawCircularObject :: Bounds -> Colour -> Picture
drawCircularObject bounds colour
    = (coloured colour circ)
      where
        (diameter, _) = boundsWidthHeight bounds
        (Point centerX centerY) = boundsCenter bounds
        circ = translated centerX centerY (solidCircle (diameter / 2))

drawGameOverScreen :: Picture
drawGameOverScreen 
  = scaled 2 2 (coloured black (lettering "Game Over!\nPress 'R' to restart"))

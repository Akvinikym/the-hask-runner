module HaskRunner.Graphics.Drawers where

import CodeWorld
import qualified Data.Text as T
import Data.Text.Read
import HaskRunner.Core

-- | All kinds of drawers are located here

-- draw player and all objects currently on the screen
drawLevel :: Level -> Picture
drawLevel level
    | isFinished level = drawGameOverScreen
    | otherwise        = drawScore 0 <> (drawPlayer (player level))
      <> foldr ((<>) . drawObject (levelPos level)) blank objectsOnScreen
      <> foldr ((<>) . drawObject 0) blank (edges level)
  where
    objectsOnScreen
        = takeWhile (onScreen level)
          (dropWhile (not . (onScreen level)) (levelMap level))

drawScore :: Integer -> Picture
drawScore score = translated scoreX scoreY scorePic
  where
    scorePic = colored white (lettering (T.pack ("score: " ++ show score)))
    scoreX = (-(screenWidth / 2 + 4))
    scoreY = (screenHeight / 2 + 3.5)

-- draw player
drawPlayer :: Player -> Picture
drawPlayer player = drawRectangularObject (pbounds player) green

-- draw a game object
drawObject :: Double -> GameObject -> Picture
drawObject offset (GameObject bounds Platform)
    = translated (-offset) 0 (drawRectangularObject bounds brown)
drawObject offset (GameObject bounds Wall)
    = translated (-offset) 0 (drawRectangularObject bounds (darker 0.5 brown))
drawObject offset (GameObject bounds Spikes)
    = translated (-offset) 0 (drawTriangularObject bounds red)
drawObject offset (GameObject bounds Coin)
    = translated (-offset) 0 (drawCircularObject bounds yellow)

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
        triangle = solidPolygon [((x1 + x3) / 2, y1), (x3, y3), (x4, y4)]

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

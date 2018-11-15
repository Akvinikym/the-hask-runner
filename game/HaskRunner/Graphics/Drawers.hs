module HaskRunner.Graphics.Drawers where

import CodeWorld
import qualified Data.Text as T
import HaskRunner.Core

-- | All kinds of drawers are located here

-- draw player and all objects currently on the screen
drawLevel :: Level -> Picture
drawLevel level = case (state level) of
    Playing  -> drawFullLevel level
    Dead     -> drawGameOverScreen level
    MainMenu -> drawMainMenu level


drawFullLevel :: Level -> Picture
drawFullLevel level 
    = drawScore level
        <> (drawPlayer (player1 level))
        <> (drawPlayer (player2 level))
        <> foldr ((<>) . drawObject (levelPos level)) blank objectsOnScreen
        <> foldr ((<>) . drawObject 0) blank (edges level)
    where
        objectsOnScreen
            = takeWhile (onScreen level)
                (dropWhile (not . (onScreen level)) (levelMap level))

drawGameOverScreen :: Level -> Picture
drawGameOverScreen level
  = scaled 2 2 ((coloured black finalMessage)
    <> (translated 0 (-2) (lettering finalScore1))
    <> (translated 0 (-4) (lettering finalScore2)))
  where
    score1 = gameScore level (player1 level)
    score2 = gameScore level (player2 level)
    finalMessage = lettering "Game Over! Press 'R' to restart"
    finalScore1 = T.pack("Final score of player 1: " ++  show score1)
    finalScore2 = T.pack("Final score of player 2: " ++  show score2)

drawMainMenu :: Level -> Picture
drawMainMenu _ = scaled 2 2 ((coloured black entryMessage))
  where
    entryMessage = lettering "Welcome!\nPress 'S' to start the game!"


drawScore :: Level -> Picture
drawScore level = (translated score1X score1Y score1Pic)
    <> (translated score2X score2Y score2Pic)
  where
    score1 = gameScore level (player1 level)
    score1Pic 
        = colored white (lettering (T.pack ("player 1 score: " ++ show score1)))
    score1X = (-(screenWidth / 2 + 4))
    score1Y = (screenHeight / 2 + 3.5)

    score2 = gameScore level (player2 level)
    score2Pic 
        = colored white (lettering (T.pack ("player 2 score: " ++ show score2)))
    score2X = (screenWidth / 2 + 4)
    score2Y = (screenHeight / 2 + 3.5)

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

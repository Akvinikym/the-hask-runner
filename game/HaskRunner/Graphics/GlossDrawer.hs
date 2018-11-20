module HaskRunner.Graphics.GlossDrawer where

import GHC.Float
import Graphics.Gloss
import HaskRunner.Core

-- How much to scale the picture
scaleFactor :: Float
scaleFactor = 25

drawLevel :: Level -> Picture
drawLevel level = case (state level) of
    Playing -> drawFullLevel level
    Dead -> drawGameOverScreen level
    MainMenu -> drawMainMenu level

drawMainMenu :: Level -> Picture
drawMainMenu _
  = translate (-400) 0 ((drawTextLine "Welcome!")
    <> (translate 0 (-100) (drawTextLine "Press 'S' to start the game!")))

drawTextLine :: String -> Picture
drawTextLine text = scale 0.4 0.4 (Text text)

drawGameOverScreen :: Level -> Picture
drawGameOverScreen level
  = translate (-400) 0 $ (drawTextLine finalMessage)
    <> (translate 0 (-100) (drawTextLine finalScore1))
    <> (translate 0 (-200) (drawTextLine finalScore2))
  where
    score1 = gameScore level (player1 level)
    score2 = gameScore level (player2 level)
    finalMessage = "Game Over! Press 'R' to restart"
    finalScore1 = "Final score of player 1: " ++  show score1
    finalScore2 = "Final score of player 2: " ++  show score2

drawFullLevel :: Level -> Picture
drawFullLevel level =
    scale scaleFactor scaleFactor ((drawPlayer (player1 level))
    <> (drawPlayer (player2 level))
    <> foldMap (drawObject (levelPos level)) (objectsOnScreen level)
    <> foldMap (drawObject 0) (edges level)) <> drawScore level
  where
      -- objectsOnScreen
      --   = takeWhile (onScreen level)
      --     (dropWhile (not . (onScreen level)) (levelMap level))

drawScore :: Level -> Picture
drawScore level = scale 0.6 0.6 (translate score1X score1Y score1Pic)
    <> scale 0.6 0.6 (translate score2X score2Y score2Pic)
  where
    score1 = gameScore level (player1 level)
    score1Pic
        = color black (drawTextLine ("player 1 score: " ++ show score1))
    score1X = double2Float (-(800))
    score1Y = double2Float (635)

    score2 = gameScore level (player2 level)
    score2Pic
        = color black (drawTextLine ("player 2 score: " ++ show score2))
    score2X = double2Float (300)
    score2Y = double2Float (635)

drawPlayer :: Player -> Picture
drawPlayer player
    | isDead player = blank
    | otherwise     = drawRectangularObject (pbounds player) green

drawRectangularObject :: Bounds -> Color -> Picture
drawRectangularObject bounds c
    = color c rect
      where
        (width, height) = boundsWidthHeight bounds
        (Point centerX centerY) = boundsCenter bounds
       -- rect = translate (double2Float centerX) (double2Float centerY) (rectangleSolid (double2Float centerX) (double2Float centerY))
        rect = translate (double2Float centerX) (double2Float centerY) (polygon (rectanglePath (double2Float width) (double2Float height)))

drawObject :: Double -> GameObject -> Picture
drawObject offset (GameObject bounds Platform)
    = translate (double2Float (-offset)) 0 (drawRectangularObject bounds orange)
drawObject offset (GameObject bounds Wall)
    = translate (double2Float (-offset)) 0 (drawRectangularObject bounds (dark orange))
drawObject offset (GameObject bounds Spikes)
    = translate (double2Float (-offset)) 0 (drawRectangularObject bounds red)
drawObject offset (GameObject bounds Coin)
    = translate (double2Float (-offset)) 0 (drawRectangularObject bounds yellow)

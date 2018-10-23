module HaskRunner
    ( main
    ) where 

import SDL
import SDL.Image
import Control.Monad (unless)

main :: IO ()
main = do
    initializeAll
    window <- createWindow "The Hask Runner" defaultWindow {
        windowInitialSize = V2 1280 720
    }
    renderer <- createRenderer window (-1) defaultRenderer
    appLoop renderer

-- | Main game loop
appLoop :: Renderer -> IO ()
appLoop renderer = do
    events <- pollEvents
    let eventIsQPress event =
          case eventPayload event of
            KeyboardEvent keyboardEvent ->
                keyboardEventKeyMotion keyboardEvent == Pressed &&
                keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
            _ -> False
        qPressed = any eventIsQPress events
    rendererDrawColor renderer $= V4 0 0 255 255
    clear renderer
    present renderer
    unless qPressed (appLoop renderer)

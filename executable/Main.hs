{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified GlossRunner

main :: IO ()
main = GlossRunner.mainLoop

-- main :: IO ()
-- main = httpBS "http://example.com" >>= B8.putStrLn . getResponseBody

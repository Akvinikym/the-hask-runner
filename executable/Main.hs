{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified GlossRunner
import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = GlossRunner.mainLoop

-- main :: IO ()
-- main = httpBS "http://example.com" >>= B8.putStrLn . getResponseBody

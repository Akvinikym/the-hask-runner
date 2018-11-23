{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty.Internal.Types (ActionT)
import Control.Monad.IO.Class
import Web.Scotty
import GHC.Generics
import qualified Data.Aeson as A
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.ByteString
import qualified Data.ByteString.Char8 as BS
import qualified Database.Redis as Redis
import Debug.Trace
import Data.Char

import Data.Monoid (mconcat)

data Score = Score {
    name :: String
  , score :: Integer
} deriving (Generic)

instance A.ToJSON Score where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.FromJSON Score

scores :: [Score]
scores = [(Score "hello" 30)]

main :: IO ()
main = do
    conn <- Redis.checkedConnect Redis.defaultConnectInfo

    scotty 3000 $ do
        get "/score" $ do
            scores <- liftIO $ getAllScores conn
            json scores
        get "/set_score/:name/:score" $ do
            name <- param "name"
            score <- param "score"
            status <- liftIO $ insertScore conn name score
            html (TL.pack (show status))

insertScore :: Redis.Connection -> ByteString -> ByteString -> IO ()
insertScore conn name score = Redis.runRedis conn $ do
  status <- Redis.sadd "scores" [name `append` ":" `append` score]
  return ()

getAllScores :: Redis.Connection -> IO [Score]
getAllScores conn = unpackScores <$> getScores conn

getScores :: Redis.Connection -> IO (Either Redis.Reply [ByteString])
getScores conn = Redis.runRedis conn $ Redis.smembers "scores"

unpackScores :: Either Redis.Reply [ByteString] -> [Score]
unpackScores (Left (Redis.Error err)) = [(Score (show err) 30)]
unpackScores (Right scores) = makeScore <$> unpackScore <$> scores
  where
    unpackScore s = show <$> BS.splitWith (==':') s
    makeScore [x, y] = Score x (readInt y)
    readInt :: String -> Integer
    readInt y = case readEither (TL.pack (Prelude.filter isDigit y)) of
      Left e -> (-1)
      Right i -> i


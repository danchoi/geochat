{-# LANGUAGE OverloadedStrings #-}

module GeoChat.EventProcessor where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad (forM_)
import GeoChat.Types
import Database.PostgreSQL.Simple


-- TODO Change these functions to work with PostgresQL

connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo { connectDatabase = "geochat"
                                 , connectUser = "choi" }
dbconn :: IO Connection
dbconn = connect connectInfo

clientExists :: Connection -> Text -> IO Bool
clientExists conn nickname = do
    xs <- query conn "select client_id, nickname from clients where nickname = ?" [nickname]
    forM_ xs $ \(cid, cnickname) -> 
      putStrLn $ show (cid :: Int) ++ " " ++  cnickname 
    return (length xs == 1)





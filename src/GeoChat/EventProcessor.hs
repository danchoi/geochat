{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GeoChat.EventProcessor where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad (forM_, liftM)
import GeoChat.Types
import Database.PostgreSQL.Simple


connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo { connectDatabase = "geochat"
                                 , connectUser = "choi" }
dbconn :: IO Connection
dbconn = connect connectInfo

{-
clientExists :: Connection -> Text -> IO Bool
clientExists conn nickname = do
    xs <- query conn "select client_id, nickname from clients where nickname = ?" [nickname]
    forM_ xs $ \(cid, cnickname) -> 
        putStrLn $ show (cid :: Int) ++ " " ++  cnickname 
    return (length xs == 1)
-}

createClient :: Connection -> Text -> IO Client
createClient conn newNick = do
    let q = "insert into clients (nickname) values (?) returning client_id, nickname"
    xs@(x:_) :: [(Int, Text)] <- query conn q [newNick]
    return (Client {clientId = (fst x) }) 



processMsg :: Connection -> Maybe Client -> MessageFromClient -> IO [MessageFromServer]

processMsg conn _ ListActiveRooms = undefined

processMsg conn (Just client) (ChangeNickname newname) = undefined

processMsg conn (Just client) (CreateRoom (lat, lng)) = do
  let cid = clientId client
      q = "insert into rooms (lat, lng) values (?, ?) returning room_id"
  xs :: [Only Int] <- query conn q (lat, lng)
  let r = UpdatedRoom (Room { roomId = (fromOnly $ head xs) , latLng = (lat, lng) , numParticipants = 0})
  return [r]

{-
processMsg conn (Just client) (ChangeRoom maybeRoomId) = do
  execute conn "update clients set room_id = ? where client_id = ?" (rid, (clientId client))
  xs :: [(Double, Double)] <- query conn "select lat, lng from rooms where room_id = ?" [rid]
  let latLng = head xs 
  let room = Room { roomId = rid, latLng = latLng, numParticipants = 1 }
  return $ [UpdatedRoom room]
-}

processMsg conn client (PostMessage msg) = undefined



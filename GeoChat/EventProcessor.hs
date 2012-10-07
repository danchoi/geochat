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

createClient :: Connection -> IO Client
createClient conn = do
    let q = "insert into clients (nickname) values ('anon') returning client_id, nickname"
    xs@(x:_) :: [(Int, Text)] <- query_ conn q 
    return (Client { clientId = (fst x)
                   , nickName = Just ("anon" :: Text)
                   , clientLatLng = Nothing
                   , clientRoomId = Nothing
                   }) 

-- makes Client record from current database information
refreshClient :: Connection -> Client -> IO Client
refreshClient conn client = do
    let q = "select lat, lng, room_id from clients where client_id = ?" 
    xs@((mlat, mlng, mrid):_) :: [(Maybe Double, Maybe Double, Maybe Int)] <- query conn q (Only $ clientId client)
    let latLng = case (mlat,mlng) of 
                   (Just lat, Just lng) -> Just (lat, lng)
                   otherwise -> Nothing
                  
    return (client {clientLatLng = latLng, clientRoomId = mrid})


processMsg :: Connection -> Client -> MessageFromClient -> IO [MessageFromServer]

processMsg conn _ ListActiveRooms = do
  let q = "select room_id, rooms.lat, rooms.lng, count(*) from rooms inner join clients using(room_id) group by room_id" 
  xs <- query_ conn q
  let r = map (\(a, b, c, d) -> UpdatedRoom $ Room { roomId = a, latLng = (b, c), numParticipants = d }) xs
  return r

processMsg conn client (LocationUpdated (lat, lng)) = do
  let q = "update clients set lat = ?, lng = ? where client_id = ?" 
  execute conn q (lat, lng, clientId client)
  r <- liftM UpdatedClient $ refreshClient conn client
  return [r]

processMsg conn client (ChangeNickname newname) = do
  let q = "update clients set nickname = ? where client_id = ?" 
  execute conn q (newname, clientId client)
  r <- liftM UpdatedClient $ refreshClient conn client
  return [r]

processMsg conn client (CreateRoom (lat, lng)) = do
  let cid = clientId client
      q = "insert into rooms (lat, lng) values (?, ?) returning room_id"
  xs :: [Only Int] <- query conn q (lat, lng)
  let r = UpdatedRoom (Room { roomId = (fromOnly $ head xs) , latLng = (lat, lng) , numParticipants = 0})
  return [r]

{-
processMsg conn client (ChangeRoom maybeRoomId) = do
  execute conn "update clients set room_id = ? where client_id = ?" (rid, (clientId client))
  xs :: [(Double, Double)] <- query conn "select lat, lng from rooms where room_id = ?" [rid]
  let latLng = head xs 
  let room = Room { roomId = rid, latLng = latLng, numParticipants = 1 }
  return $ [UpdatedRoom room]
-}

processMsg conn client (PostMessage msg) = undefined



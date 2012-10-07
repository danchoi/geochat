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

refreshClient :: Connection -> Client -> IO Client
refreshClient conn client = do
    let q = "select lat, lng, room_id from clients where client_id = ?" 
    xs@((mlat, mlng, mrid):_) :: [(Maybe Double, Maybe Double, Maybe Int)] <- query conn q (Only $ clientId client)
    let latLng = case (mlat,mlng) of 
                   (Just lat, Just lng) -> Just (lat, lng)
                   otherwise -> Nothing
                  
    return (client {clientLatLng = latLng, clientRoomId = mrid})

findRoom :: Connection -> RoomId -> IO Room
findRoom conn rid = do
    let q = "select lat, lng, count(*) from clients where room_id = ?"
    ((lat, lng, count):_) :: [(Double, Double, Int)] <- query conn q (Only rid)
    return (Room {roomId = rid, latLng = (lat, lng), numParticipants = count})

refreshRoom :: Connection -> Room -> IO Room
refreshRoom conn room = findRoom conn (roomId room)

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
  let q = "insert into rooms (lat, lng) values (?, ?) returning room_id"
  ((Only rid):_) :: [Only Int] <- query conn q (lat, lng)
  processMsg conn client (ChangeRoom rid)

processMsg conn client (ChangeRoom rid) = do
  maybeOldRoomId <- liftM clientRoomId $ refreshClient conn client
  execute conn "update clients set room_id = ? where client_id = ?" (rid, (clientId client))
  c <- liftM UpdatedClient $ refreshClient conn client
  r <- liftM UpdatedRoom $ findRoom conn rid
  return $ [c, r]

processMsg conn client Leave = do
  maybeOldRoomId <- liftM clientRoomId $ refreshClient conn client
  execute conn "update clients set room_id = null, exited = now()  where client_id = ?" (Only $ clientId client)
  -- TODO refresh the room just left
  return $ []



processMsg conn client (PostMessage msg) = undefined



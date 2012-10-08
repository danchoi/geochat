{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GeoChat.EventProcessor where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad (forM_, liftM)
import GeoChat.Types
import Database.PostgreSQL.Simple
import Data.Time.Clock

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
                   , nickName = "anon"
                   , clientLatLng = Nothing
                   , clientRoomId = Nothing
                   }) 

refreshClient :: Connection -> Client -> IO Client
refreshClient conn client = do
    let q = "select nickname, lat, lng, room_id from clients where client_id = ?" 
    xs@((nickname, mlat, mlng, mrid):_) :: [(Text, Maybe Double, Maybe Double, Maybe Int)] <- query conn q (Only $ clientId client)
    let latLng = case (mlat,mlng) of 
                   (Just lat, Just lng) -> Just (lat, lng)
                   otherwise -> Nothing
    return (client {nickName = nickname, clientLatLng = latLng, clientRoomId = mrid})

findRoom :: Connection -> RoomId -> IO Room
findRoom conn rid = do
    let q = "select max(rooms.lat), max(rooms.lng), count(clients.client_id) from rooms left outer join clients using (room_id) where room_id = ?"
    ((lat, lng, count):_) :: [(Double, Double, Int)] <- query conn q (Only rid)
    return (Room {roomId = rid, latLng = (lat, lng), numParticipants = count})

refreshRoom :: Connection -> Room -> IO Room
refreshRoom conn room = findRoom conn (roomId room)

processMsg :: Connection -> Client -> MessageFromClient -> IO [MessageFromServer]

processMsg conn _ ListActiveRooms = do
  let q = "select room_id, max(rooms.lat), max(rooms.lng), count(*) from rooms inner join clients using(room_id) group by room_id" 
  xs <- query_ conn q
  let r = map (\(a, b, c, d) -> UpdatedRoom (Room { roomId = a, latLng = (b, c), numParticipants = d }) "initialize" ) xs 
  return r

processMsg conn client (LocationUpdated (lat, lng)) = do
  let q = "update clients set lat = ?, lng = ? where client_id = ?" 
  execute conn q (lat, lng, clientId client)
  r <- liftM UpdatedClient $ refreshClient conn client
  return [r]

processMsg conn client (ChangeNickname newname) = do
  let q = "update clients set nickname = ? where client_id = ?" 
  execute conn q (newname, clientId client)
  client' <- refreshClient conn client
  r <- liftM UpdatedClient $ refreshClient conn client'
  return [r]

-- TODO if close to existing live room, Join that room
processMsg conn client (CreateRoom (lat, lng)) = do
  let q0 = "select rooms.room_id, rooms.lat, rooms.lng, \
      \ST_Distance(ST_Transform(ST_GeomFromText('POINT(' || ? || ' ' || ? || ')', 4326), 2163 ), rooms.geom) dist \
      \from rooms where \
      \ST_Distance(ST_Transform(ST_GeomFromText('POINT(' || ? || ' ' || ? || ')', 4326), 2163 ), rooms.geom) < 700 \
      \and room_id in (select room_id from rooms inner join clients using(room_id) group by room_id) \
      \order by dist asc"
  xs :: [(Int, Double, Double, Double)] <- query conn q0 (lng, lat, lng, lat)
  case xs of
    ((rid,lat,lng,dist):_) -> do
      putStrLn $ "Instead of creating, nearby room " ++ (show dist) ++ " meters close, rid "  ++ (show rid) 
      processMsg conn client (JoinRoom rid)
    otherwise -> do
      putStrLn $ "Creating new room at " ++ (show lat) ++ ", " ++ (show lng)
      let q = "insert into rooms (lat, lng) values (?, ?) returning room_id"
      ((Only rid):_) :: [Only Int] <- query conn q (lat, lng)
      processMsg conn client (JoinRoom rid)

processMsg conn client (JoinRoom rid) = do
  client' <- refreshClient conn client
  let r = clientRoomId client'
  case r of 
    Just oldRid -> do
        -- client leaves a room and joins one
        case (oldRid == rid) of
          True -> return [] -- no-op
          False -> do
            execute conn "update clients set room_id = ? where client_id = ?" (rid, (clientId client))
            c <- liftM UpdatedClient $ refreshClient conn client
            rleft <- liftM UpdatedRoom $ findRoom conn oldRid 
            rjoined <- liftM UpdatedRoom $ findRoom conn rid
            return [c, rleft $ roomMessage client' "left", rjoined $ roomMessage client' "joined"]
    Nothing -> do
        execute conn "update clients set room_id = ? where client_id = ?" (rid, (clientId client))
        c<- liftM UpdatedClient $ refreshClient conn client
        r <- liftM UpdatedRoom $ findRoom conn rid
        return [c, r $ roomMessage client' "joined"]

processMsg conn client Leave = do
  client' <- refreshClient conn client
  let r = clientRoomId client'
  case r of 
    Just x -> do
      execute conn "update clients set room_id = null, exited = now() where client_id = ?" (Only $ clientId client')
      r <- liftM UpdatedRoom $ findRoom conn x
      return $ [r $ roomMessage client' "left"]
    Nothing -> do
      execute conn "update clients set room_id = null, exited = now() where client_id = ?" (Only $ clientId client')
      return $ []

processMsg conn client (PostMessage msg) = do
  client' <- refreshClient conn client
  let r = clientRoomId client'
  case r of 
    Just rid -> do -- client in a room 
      let q = "insert into messages (room_id, client_id, client_nick, content) values (?, ?, ?, ?) returning message_id, created"
      ((mid,time):_) :: [(Int, UTCTime)] <- query conn q (rid, clientId client', nickName client', msg)
      return [Broadcast client' rid msg]
    Nothing -> do
      return []

roomMessage :: Client -> Text -> Text
roomMessage c message = (nickName c) `Text.append` " " `Text.append` message


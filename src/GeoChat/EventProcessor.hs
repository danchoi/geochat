{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module GeoChat.EventProcessor where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad (forM_, liftM, forM)
import GeoChat.Types
import Database.PostgreSQL.Simple
import Data.Time.Clock
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as C

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
                   , clientRoomId = Nothing
                   }) 

refreshClient :: Connection -> Client -> IO Client
refreshClient conn client = do
    let q = "select nickname, lat, lng, room_id from clients where client_id = ?" 
    xs@((nickname, mlat, mlng, mrid):_) :: [(Text, Maybe Double, Maybe Double, Maybe Int)] <- query conn q (Only $ clientId client)
    let latLng = case (mlat,mlng) of 
                   (Just lat, Just lng) -> Just (lat, lng)
                   otherwise -> Nothing
    return (client {nickName = nickname, clientRoomId = mrid})

findRoom :: Connection -> RoomId -> IO Room
findRoom conn rid = do
    let q = "select rooms.lat, rooms.lng from rooms where room_id = ?"
    xs@((lat, lng):_) :: [(Double, Double)] <- query conn q (Only rid)
    let q2 = "select client_id, nickname from clients where room_id = ?"
    clients :: [(Int, Text)]  <- query conn q2 (Only rid)
    return (Room { roomId = rid
                 , latLng = (lat, lng)
                 , numParticipants = (length clients)
                 , clients = clients
                 })

-- tuple client
tupClient :: Client -> Client'
tupClient c = ((clientId c), (nickName c))

refreshRoom :: Connection -> Room -> IO Room
refreshRoom conn room = findRoom conn (roomId room)

makeUpdatedRoom room = UpdatedRoom (latLng room) room

processMsg :: Connection -> Client -> MessageFromClient -> IO [MessageFromServer]

processMsg conn _ (ListActiveRooms (swlat,swlng) (nelat,nelng)) = do
  -- make a polygon box from PostGIS; 5 points to make a box

 
  let q = "select rooms.room_id from rooms \
        \inner join clients using (room_id) \
        \where ST_Intersects(  \
        \   ST_Transform(ST_MakePolygon(ST_GeomFromText('LINESTRING(' || ? || ' ' || ? || ',' || ? || ' ' || ? || ',' || ? || ' ' || ? || ',' || ? || ' ' || ? || ',' || ? || ' ' || ? || ')', 4269)), 2163), \
        \   rooms.geom)  \
        \group by rooms.room_id order by rooms.created desc "
      variables = (swlng,swlat,nelng,swlat,nelng,nelat,swlng,nelat,swlng,swlat) 

  -- formatQuery conn q variables >>= C.putStrLn  -- uncomment to debug
  xs :: [Only Int] <- query conn q variables -- go around the 4 corners and end at start
  forM xs (\x -> do 
    room <- findRoom conn (fromOnly x) 
    return $ makeUpdatedRoom room InitRoom)

processMsg conn client (ChangeNickname newname) = do
  let q = "update clients set nickname = ? where client_id = ?" 
  execute conn q (newname, clientId client)
  client' <- refreshClient conn client
  case (clientRoomId client') of
      Nothing -> return []
      Just (rid) -> do
        r <- liftM makeUpdatedRoom $ findRoom conn rid
        return [r $ ChangedNickname $ tupClient client]

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
      -- putStrLn $ "Instead of creating, nearby room " ++ (show dist) ++ " meters close, rid "  ++ (show rid) 
      processMsg conn client (JoinRoom rid)
    otherwise -> do
      -- putStrLn $ "Creating new room at " ++ (show lat) ++ ", " ++ (show lng)
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
            rleft <- liftM makeUpdatedRoom $ findRoom conn oldRid 
            rjoined <- liftM makeUpdatedRoom $ findRoom conn rid
            return [rleft $ ExitRoom (tupClient client'), rjoined $ EnterRoom (tupClient client')]
    Nothing -> do
        execute conn "update clients set room_id = ? where client_id = ?" (rid, (clientId client))
        r <- liftM makeUpdatedRoom $ findRoom conn rid
        return [r $ EnterRoom (tupClient client')]

processMsg conn client Leave = do
  client' <- refreshClient conn client
  let r = clientRoomId client'
  case r of 
    Just x -> do
      execute conn "update clients set room_id = null, exited = now() where client_id = ?" (Only $ clientId client')
      r <- liftM makeUpdatedRoom $ findRoom conn x
      return $ [r $ ExitRoom $ tupClient client']
    Nothing -> do
      execute conn "update clients set room_id = null, exited = now() where client_id = ?" (Only $ clientId client')
      return $ []

processMsg conn client (PostMessage msg) = do
  client' <- refreshClient conn client
  let r = clientRoomId client'
  case r of 
    Just rid -> do -- client in a room 
      r <- findRoom conn rid
      let q = "insert into messages (room_id, client_id, client_nick, content) values (?, ?, ?, ?) returning message_id, created"
      ((mid,time):_) :: [(Int, UTCTime)] <- query conn q (rid, clientId client', nickName client', msg)
      return [Broadcast (latLng r) (tupClient client') rid msg]
    Nothing -> do
      return []

-- catchall
processMsg conn client _ = return []



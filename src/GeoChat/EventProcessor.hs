{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GeoChat.EventProcessor where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad (forM_, liftM)
import GeoChat.Types
import Database.PostgreSQL.Simple


-- TODO Change these functions to work with PostgresQL
-- TODO keep a Map of clientId :: Int to WS.sink
-- functions will look up list of client ids from DB to broadcast to 

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


-- We need database IO so this is in the IO Monad

processMsg :: Connection -> MessageFromClient -> IO MessageFromServer

processMsg conn ListActiveRooms = undefined

-- e.g. processMsg c (NewClient $ Text.pack "dan2")

processMsg conn (NewClient newNick) = do
    let q = "insert into clients (nickname) values (?) returning client_id, nickname"
    xs :: [(Int, Text)] <- query conn q [newNick]
    return $ NewClientCreated (Client { clientId = ((fst . head) xs)
                                      , nickname = newNick
                                      , clientSink = Nothing
                                      , clientRoom = Nothing })

processMsg conn (CreateRoom (lat, lng)) = do
    let q = "insert into rooms (lat, lng) values (?, ?) returning room_id"
    xs :: [Only Int] <- query conn q (lat, lng)
    return $ NewRoom (Room { roomId = (fromOnly $ head xs)
                           , latLng = (lat, lng)
                           , numParticipants = 0})

-- TODO change hardcoded numParticipants

processMsg conn (Enter cid rid) = do
    execute conn "update clients set room_id = ? where client_id = ?" (rid, cid)
    xs :: [(Double, Double)] <- query conn "select lat, lng from rooms where room_id = ?" [rid]
    let latLng = head xs 
    let room = Room { roomId = rid, latLng = latLng, numParticipants = 1 }
    return $ UpdatedRoom room

processMsg conn (Exit cid rid) = do
    execute conn "update clients set room_id = null where client_id = ?" [cid]
    -- TODO fill in number of participants
    -- DRY up code with common method to get room data?
    xs :: [(Double, Double)] <- query conn "select lat, lng from rooms where room_id = ?" [rid]
    let latLng = head xs 
    let room = Room { roomId = rid, latLng = latLng, numParticipants = 1 }
    return $ UpdatedRoom room

processMsg conn (ChangeNickname newnick) = undefined
processMsg conn (PostMessage cid msg) = undefined


-- add association between client and room in db
-- return updated Room record
-- return RoomUpdated message to clients with Room record
-- nickname should be unique per room, not per clients table in postgres
addClientToRoom :: Connection -> Client -> Room -> IO MessageFromServer
addClientToRoom conn client room = undefined

removeClientFromRoom :: Connection -> Client -> Room -> IO MessageFromServer 
removeClientFromRoom conn client room = undefined

-- save in postgres, contruct record and return NewRoom 
addRoom :: Connection -> LatLng -> IO MessageFromServer
addRoom conn latlng = undefined


-- should WS.sendSink be called here?
-- find all clients in the room and WS.sendSink to their sinks
-- Maybe need a SinkMap Map type 
-- Log message in postgresql with room_id fk, and then send to all sinks
broadcastToRoom :: Connection -> Room -> Text -> MessageFromServer
broadcastToRoom conn room message = undefined





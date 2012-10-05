{-# LANGUAGE OverloadedStrings #-}

module GeoChat.EventProcessor where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad (forM_)
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

processMsg :: MessageFromClient -> IO MessageFromServer
processMsg ListActiveRooms = undefined
processMsg (Enter cid rid) = undefined
processMsg (Exit cid rid) = undefined
processMsg (ChangeNickname newnick) = undefined
processMsg (PostMessage cid msg) = undefined


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





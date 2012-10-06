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

processMsg :: Connection -> Client -> MessageFromClient -> IO MessageFromServer

processMsg conn client ListActiveRooms = undefined

processMsg conn client (NewClient newNick) = do
    let q = "insert into clients (nickname) values (?) returning client_id, nickname"
    xs :: [(Int, Text)] <- query conn q [newNick]
    return $ NewClientCreated (Client { clientId = ((fst . head) xs)
                                      , nickname = newNick
                                      , clientSink = Nothing
                                      , clientRoom = Nothing })

processMsg conn client (CreateRoom (lat, lng)) = do
    let q = "insert into rooms (lat, lng) values (?, ?) returning room_id"
    xs :: [Only Int] <- query conn q (lat, lng)
    return $ NewRoom (Room { roomId = (fromOnly $ head xs)
                           , latLng = (lat, lng)
                           , numParticipants = 0})

-- TODO will need to send back list of possibly pair of updated rooms

processMsg conn client (ChangeRoom Nothing) = do
    execute conn "update clients set room_id = null where client_id = ?" [(clientId client)]
    -- INCOMPLETE

processMsg conn client (ChangeRoom (Just rid)) = do
    execute conn "update clients set room_id = ? where client_id = ?" (rid, (clientId client))

    xs :: [(Double, Double)] <- query conn "select lat, lng from rooms where room_id = ?" [rid]
    let latLng = head xs 
    let room = Room { roomId = rid, latLng = latLng, numParticipants = 1 }
    return $ UpdatedRooms room

processMsg conn client (ChangeNickname newname) = undefined


processMsg conn client (PostMessage msg) = undefined


-- map updateRoom to produce a list of these and then wrap in UpdatedRooms
updateRoom :: RoomId -> Room
updateRoom rid = undefined


-- processMsg conn _ = return (ErrorMessage "Unknown Error")
-- This throws and error:     Warning: Pattern match(es) are overlapped
--

{-
 
e.g. processMsg c (NewClient $ Text.pack "dan2")

-}

 

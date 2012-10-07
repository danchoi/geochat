{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (fromException)
import Control.Monad (forM_)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Network.WebSockets as WS
import GeoChat.Types
import GeoChat.JSONInstances
import GeoChat.EventProcessor
import Data.Aeson 
import Data.Text.Lazy.Encoding as E
import Database.PostgreSQL.Simple (Connection)

type ClientSink = (ClientId, WS.Sink WS.Hybi00)

type ServerState = [ClientSink]

newServerState :: ServerState 
newServerState = []

addClientSink :: ClientSink -> ServerState -> ServerState
addClientSink cs s = cs:s

removeClientSink :: ClientId -> ServerState -> ServerState
removeClientSink cid s = filter ((/= cid) . fst) $ s 

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
application state rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    sink <- WS.getSink
    sinks <- liftIO $ readMVar state
    conn <- liftIO GeoChat.EventProcessor.dbconn
    client <- liftIO $ createClient conn
    liftIO $ putStrLn $ "Created client " `mappend` (show $ clientId client) 
    liftIO $ modifyMVar_ state $ \s -> do
        let s' = addClientSink ((clientId client), sink) s
        WS.sendSink sink $ WS.textData $ "Welcome client " `mappend` (T.pack . show $ clientId client)
        --- broadcast a joined message
        return s'
    rooms <- liftIO $ processMsg conn Nothing ListActiveRooms 
    WS.sendTextData $ encode rooms 
    receiveMessage state conn client sink

receiveMessage :: WS.Protocol p => MVar ServerState -> Connection -> Client -> WS.Sink a -> WS.WebSockets p ()
receiveMessage state conn client sink = flip WS.catchWsError catchDisconnect $ do
    rawMsg <- WS.receiveData 
    let maybeClientMessage = decode rawMsg :: Maybe MessageFromClient
    case maybeClientMessage of
        Just clientMessage -> do 
            liftIO $ putStrLn $ "Processing MessageFromClient: " `mappend` (show clientMessage)
            msgsFromServer <- liftIO $ processMsg conn (Just client) clientMessage
            -- TODO broadcast this
            return ()
        Nothing -> do 
            let errMsg = (E.decodeUtf8 rawMsg)
            liftIO $ TL.putStrLn $ "Failed to decode: " `mappend`  errMsg
            return () 
    receiveMessage state conn client sink
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
            let s' = removeClientSink (clientId client) s
            putStrLn $ "Connection closed by client " ++ (show . clientId $ client)
            -- broadcast ((nickname client) `mappend` " disconnected") s'
            -- TODO REMOVE CLIENT FROM ALL ROOMS, UPDATE EXITED TIMESTAMP
            return s'
        _ -> return ()



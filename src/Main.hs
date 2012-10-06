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

main :: IO ()
main = do
    WS.runServer "0.0.0.0" 9160 $ application 

application :: WS.Request -> WS.WebSockets WS.Hybi00 ()
application rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    sink <- WS.getSink
    conn <- liftIO GeoChat.EventProcessor.dbconn
    rooms <- liftIO $ processMsg conn Nothing ListActiveRooms 
    WS.sendTextData $ encode rooms 
    establishClient conn sink

establishClient :: WS.Protocol p => Connection -> WS.Sink a -> WS.WebSockets p ()
establishClient conn sink = flip WS.catchWsError catchDisconnect $ do
    rawMsg <- WS.receiveData 
    let maybeClientMessage = decode rawMsg :: Maybe MessageFromClient
    case maybeClientMessage of
        Just (NewClient newNick) -> do 
            client <- liftIO $ createClient conn newNick
            procClientMsg conn client sink
        -- TODO: paint map
        Just _ -> do 
            liftIO $ TL.putStrLn $ "Message not allowed yet: " `mappend`  (E.decodeUtf8 rawMsg)
            establishClient conn sink
        Nothing -> do 
            let errMsg = (E.decodeUtf8 rawMsg)
            liftIO $ TL.putStrLn $ "Failed to decode: " `mappend`  errMsg
            establishClient conn sink
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> liftIO $ do
            putStrLn "CONN CLOSED"
            -- broadcast ((nickname client) `mappend` " disconnected") s'
            return ()
        _ -> return ()

procClientMsg :: WS.Protocol p => Connection -> Client -> WS.Sink a -> WS.WebSockets p ()
procClientMsg conn client sink = flip WS.catchWsError catchDisconnect $ do
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
    procClientMsg conn client sink
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> liftIO $ do
            putStrLn "CONN CLOSED"
            -- broadcast ((nickname client) `mappend` " disconnected") s'
            -- TODO REMOVE CLIENT FROM ALL ROOMS, UPDATE EXITED TIMESTAMP
            return ()
        _ -> return ()



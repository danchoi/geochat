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

main :: IO ()
main = do
    WS.runServer "0.0.0.0" 9160 $ application 

application :: WS.Request -> WS.WebSockets WS.Hybi00 ()
application rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    sink <- WS.getSink
    establishClient sink

establishClient :: WS.Protocol p => WS.Sink a -> WS.WebSockets p ()
establishClient sink = flip WS.catchWsError catchDisconnect $ do
    rawMsg <- WS.receiveData 
    let maybeClientMessage = decode rawMsg :: Maybe MessageFromClient
    db <- liftIO GeoChat.EventProcessor.dbconn
    case maybeClientMessage of
        Just (NewClient newNick) -> do 
            client <- liftIO $ createClient db newNick
            processMessageFromClient client sink
        -- TODO: paint map
        Just _ -> do 
            liftIO $ TL.putStrLn $ "Message not allowed yet: " `mappend`  (E.decodeUtf8 rawMsg)
            establishClient sink
        Nothing -> do 
            let errMsg = (E.decodeUtf8 rawMsg)
            liftIO $ TL.putStrLn $ "Failed to decode: " `mappend`  errMsg
            establishClient sink
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> liftIO $ do
            putStrLn "CONN CLOSED"
            -- broadcast ((nickname client) `mappend` " disconnected") s'
            return ()
        _ -> return ()

processMessageFromClient :: WS.Protocol p => Client -> WS.Sink a -> WS.WebSockets p ()
processMessageFromClient client sink = flip WS.catchWsError catchDisconnect $ do
    rawMsg <- WS.receiveData 
    let maybeClientMessage = decode rawMsg :: Maybe MessageFromClient
    db <- liftIO GeoChat.EventProcessor.dbconn
    case maybeClientMessage of
        Just clientMessage -> do 
            liftIO $ putStrLn $ "Processing MessageFromClient: " `mappend` (show clientMessage)
            msgsFromServer <- liftIO $ processMsg db (Just client) clientMessage
            -- TODO broadcast this
            return ()
        Nothing -> do 
            let errMsg = (E.decodeUtf8 rawMsg)
            liftIO $ TL.putStrLn $ "Failed to decode: " `mappend`  errMsg
            return () 
    processMessageFromClient client sink
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> liftIO $ do
            putStrLn "CONN CLOSED"
            -- broadcast ((nickname client) `mappend` " disconnected") s'
            -- TODO REMOVE CLIENT FROM ALL ROOMS, UPDATE EXITED TIMESTAMP
            return ()
        _ -> return ()



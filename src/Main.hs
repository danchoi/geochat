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

data ServerState = ServerState { clients :: [Client] }

newServerState :: ServerState
newServerState = ServerState { clients = [] }

numClients :: ServerState -> Int
numClients = length . clients

clientExists :: Client -> ServerState -> Bool
clientExists client st = any ((== (nickname client)) . nickname) $ clients st

addClient :: Client -> ServerState -> ServerState
addClient client s = s { clients = client:(clients s) }

removeClient :: Client -> ServerState -> ServerState
removeClient client s = 
  s { clients = clients' } 
  where clients' = filter ((/= (nickname client)) . nickname) $ (clients s)

broadcast :: Text -> ServerState -> IO ()
-- TODO make a new function to broadcast to rooms only
broadcast message state = do
    T.putStrLn message
    forM_ (map clientSink $ clients state) sendMsg
    where 
      sendMsg (Just sink) = WS.sendSink sink $ WS.textData message
      sendMsg _ = return ()

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
application state rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    sink <- WS.getSink
    s <- liftIO $ readMVar state
    processIncomingJSON state sink

processIncomingJSON :: WS.Protocol p => MVar ServerState -> WS.Sink a -> WS.WebSockets p ()
processIncomingJSON state sink = flip WS.catchWsError catchDisconnect $ do
    rawMsg <- WS.receiveData 
    let maybeClientMessage = decode rawMsg :: Maybe MessageFromClient
    case maybeClientMessage of
        Just clientMessage -> do 
            db <- liftIO GeoChat.EventProcessor.dbconn
            -- TODO use return val from this to send JSON back
            -- processMsg db clientMessage
            return ()
        _ -> do 
              let errMsg = (E.decodeUtf8 rawMsg)
              liftIO $ TL.putStrLn $ "Failed to decode: " `mappend`  errMsg
              return () 
    processIncomingJSON state sink
      
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
            putStrLn "CONN CLOSED"

            -- let s' = removeClient client s
            -- broadcast ((nickname client) `mappend` " disconnected") s'

            return s
        _ -> return ()

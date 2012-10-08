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

import Snap.Core
import Control.Applicative

import Snap.Http.Server.Config
import Snap.Http.Server 
import Snap.Util.FileServe
import Network.WebSockets.Snap
import qualified Snap.Core as Snap
import qualified Snap.Internal.Http.Types as Snap
import qualified Snap.Types.Headers as Headers
import Data.List (foldl')
import qualified Data.Text.Encoding as TE
import Data.Monoid

simpleConfig :: Config m a
simpleConfig = foldl' (\accum new -> new accum) emptyConfig base where
    base = [hostName, accessLog, errorLog, locale, port, ip, cert, key, compr, verbose]
    hostName = setHostname (bsFromString "localhost")
    accessLog = setAccessLog (ConfigFileLog "log/access.log")
    errorLog = setErrorLog (ConfigFileLog "log/error.log")
    locale = setLocale "US"
    port = setSSLPort 9160
    ip = setSSLBind (bsFromString "127.0.0.1")
    cert = setSSLCert "server.crt"
    key = setSSLKey "server.key"
    compr = setCompression True
    verbose = setVerbose True
    bsFromString = TE.encodeUtf8 . T.pack


main :: IO ()
main = do
    state <- newMVar newServerState
    httpServe (setPort 9160 mempty) $ site state

site :: MVar ServerState -> Snap ()
site state = ifTop (writeBS "hello") <|> 
    route [ ("ws", runWebSocketsSnap $ application state) ] <|>
    dir "static" (serveDirectory "public")

type ClientSink = (ClientId, WS.Sink WS.Hybi00)

type ServerState = [ClientSink]

newServerState :: ServerState 
newServerState = []

addClientSink :: ClientSink -> ServerState -> ServerState
addClientSink cs s = cs:s

removeClientSink :: ClientId -> ServerState -> ServerState
removeClientSink cid s = filter ((/= cid) . fst) $ s 

broadcast :: [MessageFromServer] -> ServerState -> IO ()
broadcast ms clients = do
  forM_ clients $ \(_, clientSink) -> WS.sendSink clientSink $ WS.textData $ encode ms

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
        WS.sendSink sink $ WS.textData $ encode $ Handshake $ clientId client
        --- broadcast a joined message
        return s'
    rooms <- liftIO $ processMsg conn client ListActiveRooms 
    WS.sendTextData $ encode rooms 
    receiveMessage state conn client sink

receiveMessage :: WS.Protocol p => MVar ServerState -> Connection -> Client -> WS.Sink a -> WS.WebSockets p ()
receiveMessage state conn client sink = flip WS.catchWsError catchDisconnect $ do
    rawMsg <- WS.receiveData 
    
    case (decode rawMsg :: Maybe MessageFromClient) of
        Just clientMessage -> do 
            liftIO $ putStrLn $ "Processing MessageFromClient: " `mappend` (show clientMessage)
            msgsFromServer <- liftIO $ processMsg conn client clientMessage
            liftIO $ putStrLn $ "Sending MessageFromServer: " `mappend` (show msgsFromServer)
            liftIO $ readMVar state >>= broadcast msgsFromServer
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
            putStrLn $ "Sinks left: " ++ ((show . length) s')
            msgsFromServer <- liftIO $ processMsg conn client Leave
            liftIO $ broadcast msgsFromServer s'
            return s'
        _ -> return ()



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
import qualified Data.Map as M

import Control.OldException

simpleConfig :: Config m a
simpleConfig = foldl' (\accum new -> new accum) emptyConfig base where
    base = [hostName, accessLog, errorLog, locale, port, ip, verbose]
    hostName = setHostname (bsFromString "localhost")
    accessLog = setAccessLog (ConfigFileLog "log/access.log")
    errorLog = setErrorLog (ConfigFileLog "log/error.log")
    locale = setLocale "US"
    port = setPort 9160
    ip = setBind (bsFromString "127.0.0.1")
    verbose = setVerbose True
    bsFromString = TE.encodeUtf8 . T.pack


main :: IO ()
main = do
    state <- newMVar newServerState
    -- httpServe simpleConfig $ site state  -- run with snap
    WS.runServer "0.0.0.0" 9160 $ application state  -- run without snap

site :: MVar ServerState -> Snap ()
site state = ifTop (writeBS "hello") <|> 
    route [ ("ws", runWSSnap state) ] <|>
    dir "static" (serveDirectory "public")

runWSSnap :: MVar ServerState -> Snap ()
runWSSnap state = do 
  setTimeout 1
  runWebSocketsSnap $ application state

type Bounds = (LatLng,LatLng)
type ClientSink = (ClientId, (Maybe Bounds, WS.Sink WS.Hybi00)) 

type ServerState = M.Map ClientId (Maybe Bounds, WS.Sink WS.Hybi00)

newServerState :: ServerState 
newServerState = M.empty

addClientSink :: ClientSink -> ServerState -> ServerState
addClientSink cs@(cid, (_,sink)) s = M.insert cid (Nothing,sink) s

removeClientSink :: ClientId -> ServerState -> ServerState
removeClientSink cid s = M.delete cid s 

updateClientSinkBounds :: Client -> LatLng -> LatLng -> ServerState -> ServerState
updateClientSinkBounds c sw ne s = 
  M.update f (clientId c) s
    where f (_, sink) = Just (Just (sw, ne), sink)

-- inefficient, change
broadcast :: [MessageFromServer] -> MVar ServerState -> IO ()
broadcast ms state = do
  clientSinks <- readMVar state
  forM_ (M.toList clientSinks) $ \c -> mapM (send state c) ms
  return ()

singlecast :: [MessageFromServer] -> WS.Sink WS.Hybi00 -> IO ()
singlecast ms sink = 
    WS.sendSink sink $ WS.textData $ encode ms

sendEncoded :: WS.Sink WS.Hybi00 -> MessageFromServer -> IO ()
sendEncoded sink message = WS.sendSink sink $ WS.textData $ encode message

inBounds ((swlat,swlng), (nelat,nelng)) (lat, lng) =
  lat > swlat && lat < nelat && lng > swlng && lng < nelng

refuseSend cid m bounds = 
  -- putStrLn $ "Client " ++ (show cid) ++ " is out of bounds"
  return ()

-- TODO change this to use faster lookup by key and calculate target clients with PostGIS

send :: MVar ServerState -> ClientSink -> MessageFromServer -> IO ()
send state (cid, (Just bounds, sink)) m = do
  r <- try (
    case m of 
      UpdatedRoom latLng _ _  ->
        if (inBounds bounds latLng) then sendEncoded sink m else refuseSend cid m bounds
      Broadcast latLng _ _ _  ->
        if (inBounds bounds latLng) then sendEncoded sink m else refuseSend cid m bounds
      otherwise -> sendEncoded sink m)
  case r of 
      Left e -> do
          liftIO $ modifyMVar_ state $ \s -> do
              let s' = removeClientSink cid s
              return s'
          putStrLn $ "Error sending sink for client " ++ (show cid) ++ ". Removing sink."
      Right _ ->
          putStrLn "Successfully send sink"
 
send state (cid,(Nothing,_)) _ = putStrLn $ "No send; client " ++ (show cid) ++ " has no latLng"

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
        let s' = addClientSink (clientId client, (Nothing, sink)) s
        WS.sendSink sink $ WS.textData $ encode $ Handshake $ clientId client
        return s'
    receiveMessage state conn client sink

receiveMessage :: WS.Protocol p => MVar ServerState -> Connection -> Client -> WS.Sink WS.Hybi00 -> WS.WebSockets p ()
receiveMessage state conn client sink = flip WS.catchWsError catchDisconnect $ do
    rawMsg <- WS.receiveData 
    case (decode rawMsg :: Maybe MessageFromClient) of
        Just (MapBoundsUpdated sw ne) -> do 
            liftIO $ modifyMVar_ state $ \s -> do
                let s' = updateClientSinkBounds client sw ne s
                return s'
        Just m@(ListActiveRooms sw ne) -> do 
            msgsFromServer <- liftIO $ processMsg conn client m
            liftIO $ singlecast msgsFromServer sink
        Just clientMessage -> do 
            msgsFromServer <- liftIO $ processMsg conn client clientMessage
            liftIO $ broadcast msgsFromServer state
            return ()
        Nothing -> do 
            let errMsg = (E.decodeUtf8 rawMsg)
            liftIO $ TL.putStrLn $ "Failed to decode: " `mappend`  errMsg
            return () 
    receiveMessage state conn client sink
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> do 
            liftIO $ modifyMVar_ state $ \s -> do
                let s' = removeClientSink (clientId client) s
                putStrLn $ "Connection closed by client " ++ (show . clientId $ client)
                putStrLn $ "Sinks left: " ++ ((show . M.size) s')
                return s'
            msgsFromServer <- liftIO $ processMsg conn client Leave
            liftIO $ broadcast msgsFromServer state
        _ -> do 
            liftIO $ putStrLn "Uncaught Error"
            return ()



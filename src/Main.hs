{-# LANGUAGE OverloadedStrings, StandaloneDeriving, ScopedTypeVariables  #-}

module Main where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (fromException)
import Control.Monad (forM_, liftM)
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

import qualified Snap.Internal.Http.Types as Snap
import Network.WebSockets.Snap

import Data.List (foldl')
import qualified Data.Text.Encoding as TE
import qualified Data.Map as M

import Control.OldException

-- Twitter and OAuth

import qualified Network.OAuth.Consumer as OA
import Network.OAuth.Http.Request
import Network.OAuth.Http.Response
import Network.OAuth.Http.CurlHttpClient
import qualified Network.OAuth.Http.PercentEncoding as OA
import Data.Maybe (fromJust)
import Control.Monad.Error (runErrorT)



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
    httpServe simpleConfig $ site state  -- run with snap
    -- WS.runServer "0.0.0.0" 9160 $ application state  -- run without snap


------------------------------------------------------------------------

site :: MVar ServerState -> Snap ()
site state = ifTop (writeBS "hello") <|> 
    route [ ("login", loginWithTwitterHandler) ] <|>
    route [ ("ws", runWSSnap state) ] <|>
    dir "static" (serveDirectory "public")

runWSSnap :: MVar ServerState -> Snap ()
runWSSnap state = do 
  runWebSocketsSnap $ application state


------------------------------------------------------------------------


type Bounds = (LatLng,LatLng)
type ClientSink = (ClientId, (Maybe Bounds, WS.Sink WS.Hybi10)) 

type ServerState = M.Map ClientId (Maybe Bounds, WS.Sink WS.Hybi10)

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
  forM_ (M.toList clientSinks) $ \c -> mapM (sendMessageIfClientInBounds state c) ms
  return ()

singlecast :: [MessageFromServer] -> WS.Sink WS.Hybi10 -> IO ()
singlecast ms sink = 
    WS.sendSink sink $ WS.textData $ encode ms

sendEncoded :: WS.Sink WS.Hybi10 -> MessageFromServer -> IO ()
sendEncoded sink message = do 
  -- putStrLn $ "Sending " ++ (show message)
  WS.sendSink sink $ WS.textData $ encode message

inBounds ((swlat,swlng), (nelat,nelng)) (lat, lng) =
  lat > swlat && lat < nelat && lng > swlng && lng < nelng

refuseSend :: ClientId -> MessageFromServer -> Bounds -> IO ()
refuseSend cid m bounds = do
  putStrLn $ "Client " ++ (show cid) ++ " is out of bounds for message " ++ (show m)
  return ()

-- TODO change this to use faster lookup by key and calculate target clients with PostGIS

sendMessageIfClientInBounds :: MVar ServerState -> ClientSink -> MessageFromServer -> IO ()
sendMessageIfClientInBounds state (cid, (Just bounds, sink)) m = do
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
              putStrLn $ "Removed client sink " ++ (show cid)
              return s'
          putStrLn $ "Error sending sink for client " ++ (show cid) ++ ". Removing sink."
      Right _ -> do
          putStrLn $ "Successfully sent " ++ (show m) ++ " to client " ++ (show cid)
          return ()
 
sendMessageIfClientInBounds state (cid,(Nothing,_)) _ = putStrLn $ "No sendMessage; client " ++ (show cid) ++ " has no latLng"

application :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi10 ()
application state rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    WS.spawnPingThread 30  :: WS.WebSockets WS.Hybi10 ()
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

receiveMessage :: WS.Protocol p => MVar ServerState -> Connection -> Client -> WS.Sink WS.Hybi10 -> WS.WebSockets p ()
receiveMessage state conn client sink = flip WS.catchWsError catchDisconnect $ do
    rawMsg <- WS.receiveData 
    liftIO $ putStrLn $ "receiveData: " ++ (show rawMsg)
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
            liftIO $ putStrLn $ "about to broadcast: " ++ (show msgsFromServer)
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
                putStrLn $ "Removed client sink " ++ (show $ clientId client)
                putStrLn $ "Sinks left: " ++ ((show . M.size) s')
                return s'
            msgsFromServer <- liftIO $ processMsg conn client Leave
            liftIO $ broadcast msgsFromServer state
        _ -> do 
            liftIO $ putStrLn "Uncaught Error"
            return ()



------------------------------------------------------------------------
-- Twitter


deriving instance Show OA.Application
deriving instance Show OA.Token
deriving instance Read OA.Application
deriving instance Read OA.OAuthCallback

reqUrl = fromJust . parseURL $ "https://api.twitter.com/oauth/request_token"
accUrl = fromJust . parseURL $ "https://api.twitter.com/oauth/access_token"
serviceUrl = fromJust . parseURL $ "http://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=danchoi"
authUrl = ("https://api.twitter.com/oauth/authorize?oauth_token=" ++) . findWithDefault ("oauth_token","") . OA.oauthParams

data Consumer = Consumer { key :: String , secret :: String } deriving (Show, Eq)


twitterApp :: IO OA.Application 
twitterApp = do
    (key:secret:callback:_) <- liftM lines $ readFile "twitter.cfg" 
    return $ OA.Application { OA.consKey = key, OA.consSec = secret, OA.callback = (OA.URL callback) }

loginWithTwitterHandler :: Snap ()
loginWithTwitterHandler = do
    --   writeLBS $ "login with Twitter"
    tapp <- liftIO twitterApp
    liftIO $ putStrLn $ "Using config " ++ (show tapp)
    reqToken :: OA.Token <- OA.runOAuthM (OA.fromApplication tapp) $ do
        s1 <- OA.signRq2 OA.HMACSHA1 Nothing reqUrl 
        OA.oauthRequest CurlClient s1
        token <- OA.getToken
        return token
    liftIO $ putStrLn $ "Received reqToken: " ++ (show reqToken)
    -- next we save the token in session and redirect user to authorization page
    return ()

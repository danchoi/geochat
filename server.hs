{-# LANGUAGE OverloadedStrings #-}
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (fromException)
import Control.Monad (forM_)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

type LatLng = (Double, Double)

data Room = Room { 
              latLng :: LatLng 
            } deriving (Show)

data Client = Client { 
                nickname :: Text
              , getSink :: WS.Sink WS.Hybi00
              , getRoom :: Maybe Room 
              }

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
broadcast message state = do
    T.putStrLn message
    forM_ (clients state) $ \client -> WS.sendSink (getSink client) $ WS.textData message

main :: IO ()
main = do
    state <- newMVar newServerState
    WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
application state rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    sink <- WS.getSink
    msg <- WS.receiveData
    s <- liftIO $ readMVar state
    case msg of
        _   | not (prefix `T.isPrefixOf` msg) ->
                WS.sendTextData ("Wrong announcement" :: Text)
            | any ($ nickname client)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    WS.sendTextData ("Name cannot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        "cannot be empty" :: Text)
            | clientExists client s ->
                WS.sendTextData ("User already exists" :: Text)
            | otherwise -> do
               liftIO $ modifyMVar_ state $ \st -> do
                   let st' = addClient client st
                   WS.sendSink sink $ WS.textData $
                       "Welcome! Users: " `mappend`
                       T.intercalate ", " (map nickname (clients st))
                   broadcast (nickname client `mappend` " joined") st'
                   return st'
               talk state client
          where
            prefix = "Hi! I am "
            client = Client { nickname = (T.drop (T.length prefix) msg), getSink = sink, getRoom = Nothing }

talk :: WS.Protocol p => MVar ServerState -> Client -> WS.WebSockets p ()
talk state client = flip WS.catchWsError catchDisconnect $ do
    msg <- WS.receiveData
    liftIO $ readMVar state >>= broadcast
        ((nickname client)  `mappend` ": " `mappend` msg)
    talk state client
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
            let s' = removeClient client s
            broadcast ((nickname client) `mappend` " disconnected") s'
            return s'
        _ -> return ()

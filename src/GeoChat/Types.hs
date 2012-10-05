{-# LANGUAGE OverloadedStrings #-}

module GeoChat.Types where

import Data.Text (Text)
import qualified Network.WebSockets as WS
import Control.Applicative
import Control.Monad (MonadPlus, mzero)
import qualified Data.HashMap.Strict as M
import Data.Aeson 

instance Show (WS.Sink a) where
    show _ = "[WS.Sink value]"

type LatLng = (Double, Double)

data Room = Room { roomId :: Int 
                 , latLng :: LatLng 
                 , numParticipants :: Int
                 } deriving (Show)

data Client = Client { clientId :: Int
                     , nickname :: Text
                     , clientSink :: Maybe (WS.Sink WS.Hybi00)
                     , clientRoom :: Maybe Room 
                     } deriving (Show)
type RoomId = Int
type ClientId = Int

data MessageFromClient = ListActiveRooms   -- TODO scope by latLng center
                       | CreateRoom LatLng
                       | Enter ClientId RoomId 
                       | Exit ClientId RoomId 
                       | NewClient Text -- nickname 
                       | ChangeNickname Text
                       | PostMessage ClientId Text deriving (Show)

data MessageFromServer = ListOfActiveRooms [Room]
                       | NewClientCreated Client
                       | RoomActivity Room
                       | BroadcastToRoom Room Text -- need to add author or make ChatMessage type
                       | NewRoom Room
                       | UpdatedRoom Room
                       | DeadRoom Room deriving (Show)

-- test in ghci:
-- (decode $ pack "{\"type\": \"Enter\", \"clientId\": 2, \"roomId\": 3}")::Maybe MessageFromClient
-- Just (Enter 2 3)
--
-- (decode $ pack "{\"type\": \"CreateRoom\", \"lat\": 43.3, \"lng\": -70.1}")::Maybe MessageFromClient

instance FromJSON MessageFromClient where
  parseJSON (Object v) | Just "Enter" <- M.lookup "type" v = Enter <$> v .: "clientId" <*> v .: "roomId"
                       | Just "Exit" <- M.lookup "type" v = Exit <$> v .: "clientId" <*> v .: "roomId"
                       | Just "NewClient" <- M.lookup "type" v = NewClient <$> v .: "nickname" 
                       | Just "CreateRoom" <- M.lookup "type" v = CreateRoom <$> ((,) <$> v .: "lat" <*>  v .: "lng")
                       | otherwise  = mzero
  parseJSON _ = mzero


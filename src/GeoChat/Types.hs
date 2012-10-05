{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GeoChat.Types where

import Data.Text (Text)
import Data.Vector (fromList)
import qualified Network.WebSockets as WS
import Control.Applicative
import Control.Monad (MonadPlus, mzero)
import qualified Data.HashMap.Strict as M
import GHC.Generics (Generic)
import Data.Aeson 

instance Show (WS.Sink a) where
    show _ = "[WS.Sink value]"

instance ToJSON (WS.Sink a) where
    toJSON _ = toJSON ()   

type LatLng = (Double, Double)

data Room = Room { roomId :: Int 
                 , latLng :: LatLng 
                 , numParticipants :: Int
                 } deriving (Generic, Show)

instance ToJSON Room

data Client = Client { clientId :: Int
                     , nickname :: Text
                     , clientSink :: Maybe (WS.Sink WS.Hybi00)
                     , clientRoom :: Maybe Room 
                     } deriving (Generic, Show)

instance ToJSON Client

type RoomId = Int
type ClientId = Int

data MessageFromClient = ListActiveRooms   -- TODO scope by latLng center
                       | NewClient Text -- nickname 
                       | CreateRoom LatLng
                       | Enter ClientId RoomId 
                       | Exit ClientId RoomId 
                       | ChangeNickname Text
                       | PostMessage ClientId Text deriving (Show)

instance FromJSON MessageFromClient where
  parseJSON (Object v) 
    | Just "NewClient" <- M.lookup "type" v = NewClient <$> v .: "nickname" 
    | Just "CreateRoom" <- M.lookup "type" v = CreateRoom <$> ((,) <$> v .: "lat" <*>  v .: "lng")
    | Just "Enter" <- M.lookup "type" v = Enter <$> v .: "clientId" <*> v .: "roomId"
    | Just "Exit" <- M.lookup "type" v = Exit <$> v .: "clientId" <*> v .: "roomId"
    | otherwise  = mzero
  parseJSON _ = mzero

-- TODO MessageFromServer needs list of Client sinks to broadcast to

data MessageFromServer = ListOfActiveRooms [Room]
                       | NewClientCreated Client
                       | NewRoom Room
                       | RoomActivity Room
                       | Broadcast Room Text -- need to add author or make ChatMessage type
                       | UpdatedRoom Room
                       | DeadRoom Room deriving (Show)

instance ToJSON MessageFromServer where
  toJSON (ListOfActiveRooms rooms) = object ["type" .= ("ListOfActiveRooms" :: Text), "rooms" .= (fromList rooms)]
  toJSON (NewClientCreated c) = object ["type" .= ("NewClientCreated" :: Text), "client" .= c]
  toJSON (NewRoom room) = object ["type" .= ("NewRoom" :: Text), "room" .= room]
  toJSON (RoomActivity room) = object ["type" .= ("RoomActivity" :: Text), "room" .= room]
  toJSON (UpdatedRoom room) = object ["type" .= ("UpdatedRoom" :: Text), "room" .= room]
  toJSON (DeadRoom room) = object ["type" .= ("DeadRoom" :: Text), "room" .= room]
  toJSON (Broadcast room text) = object ["type" .= ("BroadCast" :: Text), "room" .= room, "text" .= text]

{- 

(decode $ pack "{\"type\": \"Enter\", \"clientId\": 2, \"roomId\": 3}")::Maybe MessageFromClient
Just (Enter 2 3)

(decode $ pack "{\"type\": \"CreateRoom\", \"lat\": 43.3, \"lng\": -70.1}")::Maybe MessageFromClient
Just (CreateRoom (43.3,-70.1))

encode (NewClientCreated Client { clientId = 12, nickname = Text.pack "dan" , clientSink = Nothing , clientRoom = Nothing})
Chunk "{\"clientId\":12,\"type\":\"NewClientCreated\"}" Empty

Experiment:
encode (ListOfActiveRooms [])

encode (ListOfActiveRooms [Room { roomId = 3 , latLng = (40.2, -71.2), numParticipants = 0}])
Chunk "{\"rooms\":[{\"latLng\":[40.2,-71.2],\"numParticipants\":0,\"roomId\":3}],\"type\":\"ListOfActiveRooms\"}" Empty

encode (RoomActivity Room { roomId = 3 , latLng = (40.2, -71.2), numParticipants = 0})

-}


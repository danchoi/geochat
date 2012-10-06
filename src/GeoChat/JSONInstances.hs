{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GeoChat.JSONInstances where
import GeoChat.Types
import GHC.Generics (Generic)
import Data.Aeson 
import Data.Text (Text)
import Control.Applicative
import Control.Monad (MonadPlus, mzero)
import qualified Data.HashMap.Strict as M
import Data.Vector (fromList)


instance FromJSON MessageFromClient where
  parseJSON (Object v) 
    | Just "NewClient" <- M.lookup "type" v = NewClient <$> v .: "nickname" 
    | Just "CreateRoom" <- M.lookup "type" v = CreateRoom <$> ((,) <$> v .: "lat" <*>  v .: "lng")
    | Just "ChangeNickname" <- M.lookup "type" v = ChangeNickname <$> v .: "nickname" 
    | Just "ChangeRoom" <- M.lookup "type" v = ChangeRoom <$> v .: "roomId" 
    | Just "PostMessage" <- M.lookup "type" v = PostMessage <$> v .: "content" 
    | otherwise  = mzero
  parseJSON _ = mzero


instance ToJSON MessageFromServer where
  toJSON (ListOfActiveRooms rooms) = object ["type" .= ("ListOfActiveRooms" :: Text), "rooms" .= (fromList rooms)]
  toJSON (NewClientCreated c) = object ["type" .= ("NewClientCreated" :: Text), "client" .= c]
  toJSON (NewRoom room) = object ["type" .= ("NewRoom" :: Text), "room" .= room]
  toJSON (RoomActivity room) = object ["type" .= ("RoomActivity" :: Text), "room" .= room]
  toJSON (UpdatedRooms rooms) = object ["type" .= ("UpdatedRooms" :: Text), "rooms" .= rooms]
  toJSON (DeadRoom room) = object ["type" .= ("DeadRoom" :: Text), "room" .= room]
  toJSON (Broadcast client room text) = object ["type" .= ("BroadCast" :: Text), "client" .= client, "room" .= room, "text" .= text]


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



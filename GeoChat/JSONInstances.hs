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
    | Just "LocationUpdated" <- M.lookup "type" v = LocationUpdated <$> ((,) <$> v .: "lat" <*> v .: "lng")
    | Just "ChangeNickname" <- M.lookup "type" v = ChangeNickname <$> v .: "nickname" 
    | Just "CreateRoom" <- M.lookup "type" v = CreateRoom <$> ((,) <$> v .: "lat" <*>  v .: "lng")
    | Just "ChangeNickname" <- M.lookup "type" v = ChangeNickname <$> v .: "nickname" 
    | Just "ChangeRoom" <- M.lookup "type" v = ChangeRoom <$> v .: "roomId" 
    | Just "PostMessage" <- M.lookup "type" v = PostMessage <$> v .: "content" 
    | otherwise  = mzero
  parseJSON _ = mzero


instance ToJSON MessageFromServer where
  toJSON (UpdatedClient c) = object ["type" .= ("UpdatedClient" :: Text), "client" .= c]
  toJSON (UpdatedRoom room) = object ["type" .= ("UpdatedRoom" :: Text), "room" .= room]
  toJSON (Broadcast user room text) = object ["type" .= ("BroadCast" :: Text), "user" .= user, "room" .= room, "text" .= text]
  toJSON (ErrorMessage text) = object ["type" .= ("ErrorMessage" :: Text), "content" .= text]


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



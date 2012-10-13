{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

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
    | Just "MapBoundsUpdated" <- M.lookup "type" v = MapBoundsUpdated <$> ((,) <$> v .: "latSW" <*> v .: "lngSW") <*> ((,) <$> v .: "latNE" <*> v .: "lngNE")
    | Just "ListActiveRooms" <- M.lookup "type" v = ListActiveRooms <$> ((,) <$> v .: "latSW" <*> v .: "lngSW") <*> ((,) <$> v .: "latNE" <*> v .: "lngNE")
    | Just "ChangeNickname" <- M.lookup "type" v = ChangeNickname <$> v .: "nickname" 
    | Just "CreateRoom" <- M.lookup "type" v = CreateRoom <$> ((,) <$> v .: "lat" <*>  v .: "lng")
    | Just "ChangeNickname" <- M.lookup "type" v = ChangeNickname <$> v .: "nickname" 
    | Just "JoinRoom" <- M.lookup "type" v = JoinRoom <$> v .: "roomId" 
    | Just "PostMessage" <- M.lookup "type" v = PostMessage <$> v .: "content" 
    | otherwise  = mzero
  parseJSON _ = mzero


instance ToJSON MessageFromServer where
  toJSON (Handshake cid) = object ["type" .= ("Handshake" :: Text), "clientId" .= cid]
  toJSON (UpdatedRoom latLng room change) = 
      object ["type" .= ("UpdatedRoom" :: Text), "room" .= room, "change" .= change, "latLng" .= latLng]
  toJSON (Broadcast latLng client roomId text) = 
      object ["type" .= ("Broadcast" :: Text), "client" .= client, "roomId" .= roomId, "latLng" .= latLng, "text" .= text]
  toJSON (ErrorMessage text) = object ["type" .= ("ErrorMessage" :: Text), "content" .= text]

instance ToJSON RoomChange where
  toJSON (ChangedNickname c) = object ["type" .= ("ChangedNickname" :: Text), "client" .= c] 
  toJSON InitRoom = object ["type" .= ("InitRoom" :: Text)] 
  toJSON (EnterRoom c) = object ["type" .= ("EnterRoom" :: Text), "client" .= c] 
  toJSON (ExitRoom c) = object ["type" .= ("ExitRoom" :: Text), "client" .= c] 

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



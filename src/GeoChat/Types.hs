{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GeoChat.Types where

import Data.Text (Text)
import qualified Network.WebSockets as WS
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

type RoomId = Int
type ClientId = Int
type Nickname = Text

data Client = Client { clientId :: Int
                     , nickname :: Text
                     , clientSink :: Maybe (WS.Sink WS.Hybi00)
                     , clientRoom :: Maybe Room 
                     } deriving (Generic, Show)

instance ToJSON Client


data MessageFromClient = ListActiveRooms  -- TODO scope by latLng center
                       | NewClient Nickname -- a nickname 
                       | CreateRoom LatLng
                       | ChangeNickname Nickname 
                       | ChangeRoom (Maybe RoomId)
                       | PostMessage Text deriving (Show)

-- TODO MessageFromServer needs list of Client sinks to broadcast to

data MessageFromServer = ListOfActiveRooms [Room]
                       | NewClientCreated Client
                       | NewRoom Room
                       | RoomActivity Room
                       | Broadcast Client Room Text 
                       | UpdatedRooms [Room]
                       | DeadRoom Room 
                       | ErrorMessage { errMessage :: String } 
                       deriving (Show)

{-

What if instead of list of active rooms and updates, we bundled updated clients and rooms into same
payload

replace
  ListOfActiveRooms 
  UpdatedRooms
  NewClientCreated
  NewRoom
  DeadRoom

with 
 Updates [Room] [Client]


-}




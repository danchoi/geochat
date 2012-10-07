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

data Client = Client { clientId :: Int } deriving (Generic, Show)

instance ToJSON Client

-- User is the client-facing version of Client

data User = User { userClientId :: Int, userNickname :: Text } deriving (Generic, Show)

instance ToJSON User

data MessageFromClient = ListActiveRooms
                       | LocationUpdated LatLng
                       | CreateRoom LatLng
                       | ChangeNickname Nickname 
                       | ChangeRoom (Maybe RoomId)
                       | PostMessage Text deriving (Show)

data MessageFromServer = UpdatedUser User
                       | UpdatedRoom Room
                       | Broadcast User Room Text 
                       | ErrorMessage { errMessage :: String } 
                       deriving (Show)


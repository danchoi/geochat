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
                     , nickName :: Maybe Text
                     , clientLatLng :: Maybe LatLng
                     , clientRoomId :: Maybe Int
                     } deriving (Generic, Show)


instance ToJSON Client

data MessageFromClient = ListActiveRooms
                       | LocationUpdated LatLng
                       | CreateRoom LatLng
                       | ChangeNickname Nickname 
                       | JoinRoom RoomId
                       | LeaveRoom RoomId
                       | PostMessage Text 
                       | Leave deriving (Show)

data MessageFromServer = UpdatedClient Client
                       | UpdatedRoom Room
                       | Broadcast Client Room Text 
                       | ErrorMessage { errMessage :: String } 
                       deriving (Show)


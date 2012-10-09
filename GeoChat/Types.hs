{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

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
type Client' = (ClientId, Nickname)  
type RoomId = Int
type ClientId = Int

data Room = Room { roomId :: Int 
                 , latLng :: LatLng 
                 , numParticipants :: Int
                 , clients :: [Client']
                 } deriving (Generic, Show)

instance ToJSON Room

type Nickname = Text

data Client = Client { clientId :: Int 
                     , nickName :: Text  -- anon by default
                     , clientRoomId :: Maybe Int
                     } deriving (Generic, Show)

instance ToJSON Client

data MessageFromClient = ListActiveRooms
                       | MapBoundsUpdated LatLng LatLng
                       | CreateRoom LatLng
                       | ChangeNickname Nickname 
                       | JoinRoom RoomId
                       | PostMessage Text 
                       | Leave deriving (Show)

data RoomChange = InitRoom
                | ChangedNickname Client' 
                | EnterRoom Client' 
                | ExitRoom Client' deriving (Show)


data MessageFromServer = Handshake ClientId
                       | UpdatedRoom LatLng Room RoomChange
                       | Broadcast LatLng Client' RoomId Text
                       | ErrorMessage { errMessage :: String } 
                       deriving (Show)


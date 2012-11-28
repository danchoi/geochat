{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Types where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson 

instance ToJSON (WS.Sink a) where
    toJSON _ = toJSON ()   

type Coordinates = (Double, Double)
type Bounds = (Coordinates, Coordinates)  -- SW NE
type RoomId = Int
type ClientId = Int

data Room = Room { roomId :: Int 
                 , coordinates :: Coordinates 
                 , numParticipants :: Int
                 , clients :: [Client]
                 } deriving (Generic, Show)

instance ToJSON Room

type Nickname = Text

data Client = Client { clientId :: Int 
                     , nickName :: Text  -- anon by default
                     , clientRoomId :: Maybe Int
                     } deriving (Generic, Show)

instance ToJSON Client

data MessageFromClient = ListActiveRooms Bounds
                       | MapBoundsUpdated Bounds
                       | CreateRoom Coordinates
                       | ChangeNickname Nickname 
                       | JoinRoom RoomId
                       | PostMessage Text 
                       | Leave deriving (Show)

data RoomChange = InitRoom
                | ChangedNickname Client
                | EnterRoom Client
                | ExitRoom Client deriving (Show)


data MessageFromServer = Handshake ClientId
                       | UpdatedRoom Coordinates Room RoomChange
                       | Broadcast Coordinates Client RoomId Text
                       | ErrorMessage { errMessage :: String } 
                       deriving (Show)


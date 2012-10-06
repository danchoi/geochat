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

data MessageFromClient = ListActiveRooms  -- TODO scope by latLng center
                       | NewClient Text   -- a nickname 
                       | CreateRoom LatLng
                       | Enter ClientId RoomId 
                       | Exit ClientId RoomId 
                       | ChangeNickname ClientId Text
                       | PostMessage ClientId Text deriving (Show)

-- TODO MessageFromServer needs list of Client sinks to broadcast to

data MessageFromServer = ListOfActiveRooms [Room]
                       | NewClientCreated Client
                       | NewRoom Room
                       | RoomActivity Room
                       | Broadcast Client Room Text 
                       | UpdatedRoom Room
                       | DeadRoom Room 
                       | ErrorMessage { errMessage :: String } 
                       deriving (Show)


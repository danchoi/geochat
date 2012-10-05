{-# LANGUAGE OverloadedStrings #-}

module GeoChat.Types where

import Data.Text (Text)
import qualified Network.WebSockets as WS

type LatLng = (Double, Double)

data Room = Room { roomId :: Int 
                 , latLng :: LatLng 
                 , numParticipants :: Int
                 } deriving (Show)

data Client = Client { clientId :: Int
                     , nickname :: Text
                     , clientSink :: Maybe (WS.Sink WS.Hybi00)
                     , clientRoom :: Maybe Room 
                     }
type RoomId = Int
type ClientId = Int

data MessageFromClient = ListActiveRooms   -- TODO scope by latLng center
                       | CreateRoom LatLng
                       | Enter ClientId RoomId 
                       | Exit ClientId RoomId 
                       | NewClient Text -- nickname 
                       | ChangeNickname Text
                       | PostMessage ClientId Text

data MessageFromServer = ListOfActiveRooms [Room]
                       | NewClientCreated Client
                       | RoomActivity Room
                       | BroadcastToRoom Room Text
                       | NewRoom Room
                       | UpdatedRoom Room
                       | DeadRoom Room

                                              

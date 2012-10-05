{-# LANGUAGE OverloadedStrings #-}

module GeoChat.Types where

import Data.Text (Text)
import qualified Network.WebSockets as WS

type LatLng = (Double, Double)

data Room = Room { roomId :: Maybe Int 
                 , latLng :: LatLng 
                 , numParticipants :: Int
                 } deriving (Show)

data Client = Client { clientId :: Maybe Int
                     , nickname :: Text
                     , clientSink :: WS.Sink WS.Hybi00
                     , clientRoom :: Maybe Room 
                     }
type RoomId = Int
type ClientId = Int

data MessageFromClient = ListActiveRooms   -- TODO scope by latLng center
                       | Enter ClientId RoomId 
                       | Exit ClientId RoomId 
                       | NewClient Text -- nickname 
                       | ChangeNickname Text
                       | PostMessage ClientId Text

data MessageFromServer = ListOfActiveRooms [Room]
                       | RoomActivity Room
                       | BroadcastToRoom Room Text
                       | NewRoom Room
                       | UpdatedRoom Room
                       | DeadRoom Room

                                              

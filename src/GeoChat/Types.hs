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

data MessageFromClient = ListRooms 
                       | Enter RoomId
                       | Exit RoomId
                       | ChangeNickname Text
                       | ChatMessage Text

data MessageFromServer = ListOfRooms [Room]
                       | BroadcastToRoom Room Text

                                              

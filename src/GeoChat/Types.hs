{-# LANGUAGE OverloadedStrings #-}

module GeoChat.Types where

import Data.Text (Text)
import qualified Network.WebSockets as WS

type LatLng = (Double, Double)

data Room = Room { roomId :: Maybe Int 
                 , latLng :: LatLng 
                 } deriving (Show)

data Client = Client { clientId :: Maybe Int
                     , nickname :: Text
                     , clientSink :: WS.Sink WS.Hybi00
                     , clientRoom :: Maybe Room 
                     }
type RoomId = Int

data Message = ListRooms 
             | Enter RoomId
             | Exit RoomId
             | ChangeNickname String


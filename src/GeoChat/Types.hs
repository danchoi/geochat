{-# LANGUAGE OverloadedStrings #-}

module GeoChat.Types where

import Data.Text (Text)
import qualified Network.WebSockets as WS

type LatLng = (Double, Double)

data Room = Room { roomId :: Int 
                 , latLng :: LatLng 
                 } deriving (Show)

data Client = Client { nickname :: Text
                     , clientSink :: WS.Sink WS.Hybi00
                     , clientRoom :: Maybe Room 
                     }



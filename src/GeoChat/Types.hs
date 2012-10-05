{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivedGeneric #-}

module GeoChat.Types where

import qualified Network.WebSockets as WS

type LatLng = (Double, Double)
data Room = Room { roomId :: Int 
                 , latLng :: LatLng 
                 }

data Client = Client {

              }

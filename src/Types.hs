{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Types where

import GHC.Generics (Generic)

type Lat = Double
type Lng = Double
type Coordinates = (Lat, Lng)
type Bounds = (Coordinates, Coordinates)  -- SW NE
type RoomId = Int
type UserId = Int

data Room = Room { roomId :: Int 
                 , coordinates :: Coordinates 
                 , numParticipants :: Int
                 , users :: [User]
                 } deriving (Generic, Show)

type Nickname = String

data User = User { userId  :: Int 
                 , nickName :: String  -- anon by default
                 , userRoomId :: Maybe Int
                 } deriving (Generic, Show)



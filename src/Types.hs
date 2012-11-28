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
                 , roomCoordinates :: Coordinates 
                 , numParticipants :: Int
                 , users :: [User]
                 } deriving (Generic, Show)

data User = User { userId  :: Int 
                 , nickName :: String  -- anon by default
                 , userCoordinates :: Maybe Coordinates
                 , userBounds :: Maybe Bounds
                 , userRoomId :: Maybe Int
                 } deriving (Generic, Show)



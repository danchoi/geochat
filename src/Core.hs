{-# LANGUAGE ScopedTypeVariables #-}
module Core where
import Types
import Database.HDBC
import Database.HDBC.PostgreSQL

conn :: IO Connection
conn = connectPostgreSQL "host=localhost dbname=geochat"

countAllUsers :: IO Integer 
countAllUsers = do
    dbh <- conn
    [[r :: SqlValue]] <- quickQuery' dbh "select count(*) from users" []
    return $ fromSql r

createUser :: String -> IO User
createUser name = do
    dbh <- conn
    [r] <- quickQuery' dbh 
         ("insert into users (name) values (?) returning " ++ userFields) [toSql name]
    commit dbh
    return $ convUserRow r

allUsers :: IO [User]
allUsers = do
    dbh <- conn
    r <- quickQuery' dbh ("select " ++ userFields ++ " from users") []
    return $ map convUserRow r

setUserCoordinates :: Int -> Coordinates -> IO ()
setUserCoordinates userId (lat, lng) = do
    dbh <- conn
    run dbh 
        ("update users set coordinates = \
        \ST_Transform(ST_GeomFromText('POINT(" ++ show lng ++ " " ++ show lat ++ ")', 4326), 2163) \
        \where user_id = ?")
        [toSql userId]
    commit dbh
    return ()

getUser :: Int -> IO User
getUser uid = do
    dbh <- conn
    -- TODO Handle no results
    [r] <- quickQuery' dbh 
                     ("select " ++ userFields ++ " from users where user_id = ?")  -- st_x is lng, st_y is lat
                     [toSql uid]
    return $ convUserRow r
            
userFields = "user_id, name, \
    \st_y(st_transform(coordinates, 4326)), \
    \st_x(st_transform(coordinates, 4326)), room_id"
         
convUserRow :: [SqlValue] -> User
convUserRow [a,b,lat,lng,c] = 
    User { userId = fromSql a
         , nickName = fromSql b
         , userCoordinates = (getCoordinates (fromSql lat) (fromSql lng))
         , userBounds = Nothing
         , userRoomId = fromSql c
         }
  where getCoordinates :: Maybe Double -> Maybe Double -> Maybe Coordinates
        getCoordinates (Just lat') (Just lng') = Just (lat', lng') 
        getCoordinates _ _ = Nothing



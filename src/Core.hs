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
    [[r :: SqlValue]] <- quickQuery' dbh "insert into users (name) values (?) returning user_id" [toSql name]
    commit dbh
    return $ User (fromSql r) name Nothing Nothing 

allUsers :: IO [User]
allUsers = do
    dbh <- conn
    r <- quickQuery' dbh "select user_id, name, room_id from users" []
    return $ map convUserRow r
    where convUserRow :: [SqlValue] -> User
          convUserRow [a, b, c] = 
            User (fromSql a) (fromSql b) Nothing (fromSql c)
 
setUserCoordinates :: Int -> Coordinates -> IO ()
setUserCoordinates userId (lat, lng) = do
    dbh <- conn
    run dbh 
        ("update users set coordinates = \
        \ ST_Transform(ST_GeomFromText('POINT(" ++ show lng ++ " " ++ show lat ++ ")', 4326), 2163) where user_id = ?")
        [toSql userId]
    commit dbh
    return ()
 
getUser :: Int -> IO User
getUser uid = do
    dbh <- conn
    -- TODO Handle no results
    [[a, b, lat, lng, c]] <- quickQuery' dbh 
                     "select user_id, name, \
                     \st_y(st_transform(coordinates, 4326)), \  
                     \st_x(st_transform(coordinates, 4326)), \  
                     \room_id \
                     \from users where user_id = ?"  -- st_x is lng, st_y is lat
                     [toSql uid]
    return $ User { userId = fromSql a
                  , nickName = fromSql b
                  , userCoordinates = (getCoordinates (fromSql lat) (fromSql lng))
                  , userRoomId = fromSql c
                  }
  where getCoordinates :: Maybe Double -> Maybe Double -> Maybe Coordinates
        getCoordinates (Just lat') (Just lng') = Just (lat', lng') 
        getCoordinates _ _ = Nothing

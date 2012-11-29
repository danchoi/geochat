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

allUsersInBounds :: Bounds -> IO [User]
allUsersInBounds ((swlat, swlng), (nelat, nelng)) = do
    dbh <- conn
    r <- quickQuery' dbh 
          ("select " ++ userFields ++ " from users \
          \where ST_Intersects(  \
          \   ST_Transform(ST_MakePolygon(ST_GeomFromText('LINESTRING(' || ? || ' ' || ? || ',' || ? || ' ' || ? || ',' || ? || ' ' || ? || ',' || ? || ' ' || ? || ',' || ? || ' ' || ? || ')', 4269)), 2163), \
          \   users.coordinates)")
          variables
    return $ map convUserRow r
  where variables = map toSql [swlng, swlat, nelng, swlat, nelng, nelat, swlng, nelat, swlng, swlat] 

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

setUserBounds :: Int -> Bounds -> IO ()
setUserBounds userId ((swlat, swlng), (nelat, nelng)) = do
    dbh <- conn
    run dbh 
        ("update users set bounds = \
        \ST_Transform(ST_MakePolygon(ST_GeomFromText('LINESTRING(' || ? || ' ' || ? || ',' || ? || ' ' || ? || ',' || ? || ' ' || ? || ',' || ? || ' ' || ? || ',' || ? || ' ' || ? || ')', 4269)), 2163) \
        \where user_id = ?")
        (variables ++ [toSql userId])
    commit dbh
    return ()
  where variables = map toSql [swlng, swlat, nelng, swlat, nelng, nelat, swlng, nelat, swlng, swlat] 

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
         , userBounds = Nothing  -- Don't really need to return this to client
         , userRoomId = fromSql c
         }
  where getCoordinates :: Maybe Double -> Maybe Double -> Maybe Coordinates
        getCoordinates (Just lat') (Just lng') = Just (lat', lng') 
        getCoordinates _ _ = Nothing



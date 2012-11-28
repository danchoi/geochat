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
setUserCoordinates userId coordinates = do
    dbh <- conn
    run dbh "update users set coordinates = null where user_id = ?" [toSql userId]
    commit dbh
    return ()
 

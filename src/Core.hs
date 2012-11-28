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
  return $ User { userId = (fromSql r), nickName = name, userRoomId = Nothing }

  

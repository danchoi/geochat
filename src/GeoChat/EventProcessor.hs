module GeoChat.EventProcessor where

import GeoChat.Types

-- TODO Change these functions to work with PostgresQL

clientExists :: Client -> ServerState -> Bool
clientExists client st = any ((== (nickname client)) . nickname) $ clients st

addClient :: Client -> ServerState -> ServerState
addClient client s = s { clients = client:(clients s) }

removeClient :: Client -> ServerState -> ServerState
removeClient client s = 
  s { clients = clients' }
    where clients' = filter ((/= (nickname client)) . nickname) $ (clients s)

broadcast :: Text -> ServerState -> IO ()
-- TODO make a new function to broadcast to rooms only
broadcast message state = do
    T.putStrLn message
    forM_ (clients state) $ \client -> WS.sendSink (clientSink client) $ WS.textData message




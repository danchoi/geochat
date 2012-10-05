main = do
    c <- dbconn
    NewClientCreated c1 <- processMsg c (NewClient $ Text.pack "dan3")
    NewRoom r1 <- processMsg c $ CreateRoom (42.2, -71.2)
    UpdatedRoom room <- processMsg c (Enter (clientId c1) (roomId r1))

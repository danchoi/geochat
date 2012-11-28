{-# LANGUAGE OverloadedStrings, StandaloneDeriving, ScopedTypeVariables #-}

module TwitterOAuth 
  ( loginWithTwitterHandler
  , twitterAccessTokenHandler) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)
import Snap.Core
import Network.OAuth.Consumer 
import Network.OAuth.Http.Request 
import Network.OAuth.Http.Response
import Network.OAuth.Http.CurlHttpClient
import Network.OAuth.Http.PercentEncoding 
import Data.Maybe (fromJust)
import Control.Monad.Error (runErrorT)
import qualified Data.ByteString.Char8 as B

deriving instance Show Application
deriving instance Show Token
deriving instance Read Application
deriving instance Read OAuthCallback

reqUrl = fromJust . parseURL $ "https://api.twitter.com/oauth/request_token"
accUrl = fromJust . parseURL $ "https://api.twitter.com/oauth/access_token"
serviceUrl = fromJust . parseURL $ "http://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=danchoi"

twitterApp :: IO Application 
twitterApp = do
    (key:secret:callback:_) <- liftM lines $ readFile "twitter.cfg" 
    return $ Application { consKey = key, consSec = secret, callback = (URL callback) }

loginWithTwitterHandler :: Snap ()
loginWithTwitterHandler = do
    --   writeLBS $ "login with Twitter"
    tapp <- liftIO twitterApp
    liftIO $ putStrLn $ "Using config " ++ (show tapp)
    reqToken :: Token <- runOAuthM (fromApplication tapp) $ do
        s1 <- signRq2 HMACSHA1 Nothing reqUrl 
        oauthRequest CurlClient s1
        token <- getToken
        return token
    liftIO $ putStrLn $ "Received reqToken: " ++ (show reqToken)
    liftIO $ putStrLn $ "Token oauth params: " ++ (show $ oauthParams reqToken)
    -- TODO save the oauth creds in session
    let oauthToken = head $ find ((==) "oauth_token") $ oauthParams reqToken
    let authUrl = "https://api.twitter.com/oauth/authenticate?oauth_token=" ++ oauthToken
    redirect $ B.pack authUrl

twitterAccessTokenHandler :: Snap ()
twitterAccessTokenHandler = do
    writeLBS "twitter access verified "
    -- TODO step 3 https://dev.twitter.com/docs/auth/implementing-sign-twitter
    --    accessToken <- (signRq2 HMACSHA1 Nothing accUrl >>= oauthRequest CurlClient)
    --    liftIO $ putStrLn $ show (oauthParams accessToken)


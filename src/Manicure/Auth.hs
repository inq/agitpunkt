{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
module Manicure.Auth where

import qualified Data.ByteString.Char8          as BS
import qualified Manicure.Config                as Config
import Data.Map.Strict ((!))


--signin :: BS.ByteString


clientId     :: BS.ByteString
clientSecret :: BS.ByteString
-- ^ Client ID & Client Secret Key
(clientId, clientSecret, adminId) = (
      config ! "client_id", 
      config ! "client_secret", 
      config ! "admin_id")
  where
    config = $(Config.parseFile "config/facebook.cfg")

isAdminUser :: BS.ByteString -> Bool
-- ^ Check if the given facebook id is of admin
isAdminUser userId =
    userId == adminId

facebookMeUrl :: BS.ByteString -> BS.ByteString
-- ^ Get the user information
facebookMeUrl accessToken = BS.concat [
    "https://graph.facebook.com/v2.4/me?locale=ko_KR&access_token=",
    accessToken
  ]

oauthUrl :: BS.ByteString -> BS.ByteString
-- ^ Generate the OAuth2 URL
oauthUrl redirectUri = BS.concat [
    "https://www.facebook.com/dialog/oauth?client_id=",
    clientId,
    "&redirect_uri=",
    redirectUri
  ]

accessTokenUrl :: BS.ByteString -> BS.ByteString -> BS.ByteString
-- ^ Generate the access token URL
accessTokenUrl redirectUri code = BS.concat [
    "https://graph.facebook.com/oauth/access_token?client_id=",
    clientId,
    "&redirect_uri=",
    redirectUri,
    "&client_secret=",
    clientSecret,
    "&code=",
    code
  ]

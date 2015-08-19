{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
module Manicure.Auth where

import qualified Data.ByteString.Char8          as BS
import qualified Manicure.Config                as Config
import Data.Map.Strict ((!))


--signin :: BS.ByteString


client_id     :: BS.ByteString
client_secret :: BS.ByteString
-- ^ Client ID & Client Secret Key
(client_id, client_secret, admin_id) = (
      config ! "client_id", 
      config ! "client_secret", 
      config ! "admin_id")
  where
    config = $(Config.parseFile "config/facebook.cfg")

is_admin_user :: BS.ByteString -> Bool
-- ^ Check if the given facebook id is of admin
is_admin_user user_id =
    user_id == admin_id

facebook_me_url :: BS.ByteString -> BS.ByteString
-- ^ Get the user information
facebook_me_url access_token = BS.concat [
    "https://graph.facebook.com/v2.4/me?locale=ko_KR&access_token=",
    access_token
  ]

oauth_url :: BS.ByteString -> BS.ByteString
-- ^ Generate the OAuth2 URL
oauth_url redirect_uri = BS.concat [
    "https://www.facebook.com/dialog/oauth?client_id=",
    client_id,
    "&redirect_uri=",
    redirect_uri
  ]

access_token_url :: BS.ByteString -> BS.ByteString -> BS.ByteString
-- ^ Generate the access token URL
access_token_url redirect_uri code = BS.concat [
    "https://graph.facebook.com/oauth/access_token?client_id=",
    client_id,
    "&redirect_uri=",
    redirect_uri,
    "&client_secret=",
    client_secret,
    "&code=",
    code
  ]

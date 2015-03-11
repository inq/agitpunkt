{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Manicure.Auth where

import qualified Data.ByteString.Char8          as BS
import qualified Manicure.Config                as Config
import Data.Map.Strict ((!))

client_id     :: BS.ByteString
client_secret :: BS.ByteString
-- ^ Client ID & Client Secret Key
(client_id, client_secret) = (config ! "client_id", config ! "client_secret")
  where
    config = $(Config.parseFile "config/facebook.cfg")

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

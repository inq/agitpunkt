{-# LANGUAGE OverloadedStrings #-}
module Handler.Main where

import qualified Data.ByteString.Char8      as BS

index :: String -> String
index val = "Hello, world!"

post_test :: String -> String
post_test val = "post test"

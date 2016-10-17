{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Data.ByteString.Char8 as BS

adminUser :: BS.ByteString
adminUser = "gofiri@gmail.com"

articlePerPage :: Int
articlePerPage = 5

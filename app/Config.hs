{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Data.Text (Text)

adminUser :: Text
adminUser = "gofiri@gmail.com"

articlePerPage :: Int
articlePerPage = 10

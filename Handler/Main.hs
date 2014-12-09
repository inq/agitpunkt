{-# LANGUAGE OverloadedStrings #-}
module Handler.Main where

import qualified Data.ByteString.Char8      as BS
import qualified Manicure.Request           as R

index :: R.Request -> String
index val = show val

post_test :: R.Request -> String
post_test val = "post test"

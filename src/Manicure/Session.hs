{-# LANGUAGE OverloadedStrings #-}
module Manicure.Session where

import qualified Database.Redis                 as R
import qualified Data.Char                      as Char
import qualified Crypto.Hash.SHA256             as SHA256
import qualified Data.Unique                    as U
import qualified Data.Time.Clock.POSIX          as POSIX
import qualified Data.ByteString.Char8          as BS

data Connection = Connection R.Connection

connect :: IO (Connection)
-- ^ A wrapper to hide redis connection
connect = do
    conn <- R.connect R.defaultConnectInfo
    return $ Connection conn

generateKey :: IO BS.ByteString
-- ^ Generate a session key
generateKey = do
    t <- POSIX.getPOSIXTime
    return $ SHA256.hash . BS.pack $ show t


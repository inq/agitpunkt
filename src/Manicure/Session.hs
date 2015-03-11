{-# LANGUAGE OverloadedStrings #-}
module Manicure.Session where

import qualified Data.Char                      as Char
import qualified Crypto.Hash.SHA256             as SHA256
import qualified Data.Unique                    as U
import qualified Data.Time.Clock.POSIX          as POSIX
import qualified Data.ByteString.Char8          as BS
import qualified Manicure.Database              as DB

generateKey :: IO BS.ByteString
-- ^ Generate a session key
generateKey = do
    t <- POSIX.getPOSIXTime
    return $ SHA256.hash . BS.pack $ show t

mkSession :: IO BS.ByteString
-- ^ Make a new session data
mkSession = do
    key <- generateKey
    return key

readSession :: DB.Connection -> BS.ByteString -> IO BS.ByteString
readSession conn key = do
    DB.redisGet conn key

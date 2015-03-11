{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
module Manicure.Database where

import qualified Database.MongoDB               as M
import qualified Database.Redis                 as R
import qualified Data.ByteString.Char8          as BS
import Database.MongoDB ((=:))
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import System.IO.Pipeline
import Data.Text

data Connection = Connection R.Connection M.Pipe Text

connect :: Text -> IO Connection
-- ^ Open the new DB connection
connect db = do
    mongo <- M.connect $ M.host "127.0.0.1"
    redis <- R.connect R.defaultConnectInfo
    return $ Connection redis mongo db

redisSet :: Connection -> BS.ByteString -> BS.ByteString -> IO ()
-- ^ Write a bytestring to redis
redisSet (Connection r _ _) key value = do
    res <- R.runRedis r $ R.set key value
    return ()

redisGet :: Connection -> BS.ByteString -> IO BS.ByteString 
-- ^ Read a bytestring from redis
redisGet (Connection r _ _) key = do
     (Right (Just res)) <- R.runRedis r $ R.get key
     return res

query :: MonadIO m => Connection -> M.Action m a -> m a
-- ^ Send the given query
query (Connection _ pipe db) action = do
    M.access pipe M.master db action

close :: M.Pipe -> IO ()
-- ^ Close the connection
close pipe = do
    M.close pipe

find :: (MonadIO m, MonadBaseControl IO m)  => M.Action m [M.Document]
-- ^ ** Find articles
find = do
    (M.find $ M.select [] "articles") >>= M.rest

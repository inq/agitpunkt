{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
module Core.Database
    ( module Core.Database
    , (=:)
    ) where

import qualified Database.MongoDB as M
import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as BS
import Database.MongoDB ((=:))
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)

data Connection = Connection
  {-# UNPACK #-} !R.Connection
  {-# UNPACK #-} !M.Pipe
  {-# UNPACK #-} !M.Database

connect :: BS.ByteString -> IO Connection
-- ^ Open the new DB connection
connect db = do
    mongo <- M.connect $ M.host "127.0.0.1"
    redis <- R.connect R.defaultConnectInfo
    return $ Connection redis mongo db

runRedis :: Connection -> R.Redis a -> IO a
-- ^ A wrapper for runRedis
runRedis (Connection r _ _) = R.runRedis r

query :: MonadIO m => Connection -> M.Action m a -> m a
-- ^ Send the given query
query (Connection _ pipe db) =
    M.access pipe M.master db

close :: M.Pipe -> IO ()
-- ^ Close the connection
close = M.close

find :: (MonadIO m, MonadBaseControl IO m)  => M.Action m [M.Document]
-- ^ ** Find articles
find =
    (M.find $ M.select [] "users") >>= M.rest

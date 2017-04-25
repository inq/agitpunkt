{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
module Core.Database
    ( module Core.Database
    , (=:)
    ) where

import qualified Database.MongoDB as M
import Data.Text (Text)
import Database.MongoDB ((=:))
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)

data Connection = Connection
  {-# UNPACK #-} !M.Pipe
  {-# UNPACK #-} !M.Database

connect :: Text -> IO Connection
-- ^ Open the new DB connection
connect db = do
    mongo <- M.connect $ M.host "127.0.0.1"
    return $ Connection mongo db

query :: MonadIO m => Connection -> M.Action m a -> m a
-- ^ Send the given query
query (Connection pipe db) =
    M.access pipe M.master db

close :: M.Pipe -> IO ()
-- ^ Close the connection
close = M.close

find :: (MonadIO m, MonadBaseControl IO m)  => M.Action m [M.Document]
-- ^ ** Find articles
find =
    M.find (M.select [] "users") >>= M.rest

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
module Manicure.Database where

import qualified Database.MongoDB               as M
import Database.MongoDB ((=:))
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import System.IO.Pipeline
import Data.Text

data Connection = Connection M.Pipe Text

connect :: Text -> IO Connection
-- ^ Open the new DB connection
connect db = do
    pipe <- M.connect $ M.host "127.0.0.1"
    return $ Connection pipe db

query :: MonadIO m => Connection -> M.Action m a -> m a
-- ^ Send the given query
query (Connection pipe db) action = do
    M.access pipe M.master db action

close :: M.Pipe -> IO ()
-- ^ Close the connection
close pipe = do
    M.close pipe

find :: (MonadIO m, MonadBaseControl IO m)  => M.Action m [M.Document]
-- ^ ** Find articles
find = do
    (M.find $ M.select [] "articles") >>= M.rest

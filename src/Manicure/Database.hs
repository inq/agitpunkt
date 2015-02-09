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
connect db = do
    pipe <- M.connect $ M.host "127.0.0.1"
    return $ Connection pipe db

query :: MonadIO m => Connection -> M.Action m a -> m a
query (Connection pipe db) action = do
    M.access pipe M.master db action

close :: M.Pipe -> IO ()
close pipe = do
    M.close pipe

find :: (MonadIO m, MonadBaseControl IO m)  => M.Action m [M.Document]
find = do
    (M.find $ M.select [] "articles") >>= M.rest

runAction :: M.Action IO ()
runAction = do
    M.insertMany "users" [
        ["name" =: "p1", "details" =: ["age" =: 29]],
        ["name" =: "p2", "details" =: ["age" =: 20]]
      ]
    users >>= printDocs "hello"

users :: M.Action IO [M.Document]
users = M.rest =<< M.find (M.select [] "users") 

printDocs :: String -> [M.Document] -> M.Action IO ()
printDocs title docs = liftIO $ putStrLn title >> mapM_ (print . M.exclude ["_id"]) docs

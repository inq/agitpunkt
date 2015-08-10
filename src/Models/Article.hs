{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Models.Article where

import qualified Data.ByteString.Char8          as BS
import qualified Data.Time.Clock                as TC
import qualified Database.MongoDB               as M
import qualified Data.Bson                      as Bson
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Database.MongoDB ((=:))

data Article = Article {
  _id        :: Maybe Bson.ObjectId,
  title      :: BS.ByteString,
  content    :: BS.ByteString,
  created_at :: TC.UTCTime
}

save :: Article -> M.Action IO ()
-- ^ Save the data into the DB
save (Article _id title content created_at) = do
    M.insert "articles" [
        "title"      =: Bson.Binary title,
        "content"    =: Bson.Binary content,
        "created_at" =: created_at
      ]
    return ()

find :: (MonadIO m, MonadBaseControl IO m)  => M.Action m [M.Document]
-- ^ ** Find articles
find = do
    (M.find $ M.select [] "articles") >>= M.rest


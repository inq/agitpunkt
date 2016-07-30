{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Models.Article where

import qualified Data.ByteString.Char8          as BS
import qualified Data.Time.Clock                as TC
import qualified Database.MongoDB               as M
import qualified Data.Bson                      as Bson
import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Database.MongoDB ((=:))

data Article = Article
  { id        :: Maybe Bson.ObjectId
  , title     :: BS.ByteString
  , content   :: BS.ByteString
  , createdAt :: TC.UTCTime
  }

save :: Article -> M.Action IO ()
-- ^ Save the data into the DB
save (Article _id' title' content' createdAt') = do
    _ <- M.insert "articles" [
        "title"      =: Bson.String title',
        "content"    =: Bson.String content',
        "created_at" =: createdAt'
      ]
    return ()

find :: (MonadIO m, MonadBaseControl IO m) => M.Action m [M.Document]
-- ^ Find articles
find = do
    (M.find $ M.select [] "articles") >>= M.rest

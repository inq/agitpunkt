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
import Data.Bson ((!?))
import Data.Map ((!))

data Article = Article
  { _id       :: Maybe Bson.ObjectId
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

find :: M.Action IO [Article]
-- ^ Find articles
find = do
    res <- (M.find $ M.select [] "articles") >>= M.rest
    return $ map fromDocument res
  where
    fromDocument doc = Article
      { _id = doc !? "_id"
      , title = Bson.at "title" doc
      , content = Bson.at "content" doc
      , createdAt = Bson.at "createdAt" doc
      }

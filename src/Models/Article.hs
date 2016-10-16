{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Models.Article where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Time.Clock as TC
import qualified Database.MongoDB as Mongo
import qualified Data.Bson as Bson
import Database.MongoDB ((=:))
import Data.Bson ((!?))

data Article = Article
  { _id       :: Maybe Bson.ObjectId
  , title     :: BS.ByteString
  , content   :: BS.ByteString
  , createdAt :: TC.UTCTime
  }

save :: Article -> Mongo.Action IO ()
-- ^ Save the data into the DB
save (Article _id' title' content' createdAt') = do
    _ <- Mongo.insert "articles" [
        "title"      =: Bson.String title',
        "content"    =: Bson.String content',
        "created_at" =: createdAt'
      ]
    return ()

find :: Mongo.Action IO [Article]
-- ^ Find articles
find = do
    res <- Mongo.find (Mongo.select [] "articles") >>= Mongo.rest
    return $ map fromDocument res
  where
    fromDocument doc = Article
      { _id = doc !? "_id"
      , title = Bson.at "title" doc
      , content = Bson.at "content" doc
      , createdAt = Bson.at "created_at" doc
      }

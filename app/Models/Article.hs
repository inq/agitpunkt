{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Article
  ( Article(..)
  , count
  , save
  , list
  , update
  , find
  ) where

import           Core.Model             (Model, toDocument)
import           Data.Bson              ((!?))
import qualified Data.Bson              as Bson
import           Data.Int               (Int32)
import           Data.Text              (Text)
import qualified Data.Time.Clock        as TC
import           Database.MongoDB       ((=:))
import qualified Database.MongoDB       as Mongo
import qualified Database.MongoDB.Query as MQ
import           GHC.Generics           (Generic)

data Article = Article
  { _id       :: Maybe Bson.ObjectId
  , title     :: Text
  , content   :: Text
  , createdAt :: TC.UTCTime
  } deriving (Show, Generic)

instance Model Article

count :: Mongo.Action IO Int
-- ^ Query count of the article
count = Mongo.count (Mongo.select [] "articles")

save :: Article -> Mongo.Action IO ()
-- ^ Save the data into the DB
save (Article _id' title' content' createdAt') = do
  _ <-
    Mongo.insert
      "articles"
      [ "title" =: Bson.String title'
      , "content" =: Bson.String content'
      , "created_at" =: createdAt'
      ]
  return ()

list :: Int -> Int -> Mongo.Action IO [Article]
-- ^ List articles
list pagesize page = do
  res <-
    Mongo.find
      (Mongo.select [] "articles")
      { Mongo.sort = ["_id" =: (-1 :: Int32)]
      , Mongo.limit = fromIntegral pagesize
      , Mongo.skip = fromIntegral (pagesize * page)
      } >>=
    Mongo.rest
  return $ fromDocument <$> res
  where
    fromDocument doc =
      Article
      { _id = doc !? "_id"
      , title = Bson.at "title" doc
      , content = Bson.at "content" doc
      , createdAt = Bson.at "created_at" doc
      }

update :: Article -> Mongo.Action IO ()
update article = do
  Mongo.replace (MQ.Select ["_id" =: _id article] "articles") $
    toDocument article
  return ()

find :: Bson.ObjectId -> Mongo.Action IO (Maybe Article)
-- ^ Find articles
find oid = do
  res <- Mongo.findOne (Mongo.select ["_id" =: oid] "articles")
  return $ fromDocument <$> res
  where
    fromDocument doc =
      Article
      { _id = doc !? "_id"
      , title = Bson.at "title" doc
      , content = Bson.at "content" doc
      , createdAt = Bson.at "created_at" doc
      }

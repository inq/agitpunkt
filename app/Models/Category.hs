{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Category where

import qualified Core.Model             as Model
import           Data.Bson              ((!?))
import qualified Data.Bson              as Bson
import           Data.Text              (Text)
import qualified Database.MongoDB       as Mongo
import qualified Database.MongoDB.Query as MQ
import           GHC.Generics           (Generic)

data Category = Category
  { _id      :: Maybe Bson.ObjectId
  , name     :: Text
  , parentId :: Maybe Bson.ObjectId
  } deriving (Show, Generic)

instance Model.Model Category

find :: Mongo.Action IO [Category]
find = do
  res <- Mongo.find (Mongo.select [] "categories") >>= MQ.rest
  return $ map fromDocument res
  where
    fromDocument doc =
      Category
      { _id = doc !? "_id"
      , name = Bson.at "name" doc
      , parentId = doc !? "parent_id"
      }

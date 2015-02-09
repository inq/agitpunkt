{-# LANGUAGE OverloadedStrings #-}
module Models.Article where

import qualified Data.ByteString.Char8          as BS
import qualified Data.Time.Clock                as TC
import qualified Database.MongoDB               as M
import qualified Data.Bson                      as Bson
import Database.MongoDB ((=:))

data Article = Article {
  _id        :: Bson.ObjectId,
  title      :: BS.ByteString,
  content    :: BS.ByteString,
  created_at :: TC.UTCTime
}

save :: Article -> M.Action IO ()
save (Article _id title content created_at) = do
    M.insert "articles" [
        "title"      =: Bson.Binary title,
        "content"    =: Bson.Binary content,
        "created_at" =: created_at
      ]
    return ()

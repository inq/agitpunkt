{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Main where

import qualified Data.Bson                      as Bson
import qualified Database.MongoDB               as Mongo
import qualified Data.ByteString.Char8          as BS
import qualified Data.Time.Format               as TF
import qualified Data.Map                       as M
import qualified Manicure.Request               as Req
import qualified Manicure.Response              as Res
import qualified Manicure.Database              as DB
import qualified Models.Article                 as Article
import qualified Models.User                    as User
import qualified Manicure.Html                  as Html

index :: Res.Handler
-- ^ Render the main page
index [] db req = do
    temp <- DB.query db Article.find
    articles <- (mapM read) temp
    putStrLn $ show temp
    user <- userM
    let name = case user of
          Just (User.User _ _ name _) -> name
          Nothing -> "anonymous"
    return $ Res.success $(Html.parseFile "main/index.html.qh") []
  where
    read :: Bson.Document -> IO [BS.ByteString]
    read document = do
        title      <- Mongo.lookup "title" document
        content    <- Mongo.lookup "content" document          
        created_at <- Mongo.lookup "created_at" document
        return [
            extract title, 
            extract content,
            extract_date created_at, 
            extract_month created_at,
            extract_year created_at,
            extract_time created_at
          ]
      where
        extract (Bson.Bin (Bson.Binary a)) = a
        extract_date (Bson.UTC a) = BS.pack $ TF.formatTime TF.defaultTimeLocale "%d" a
        extract_month (Bson.UTC a) = BS.pack $ TF.formatTime TF.defaultTimeLocale "%b" a
        extract_year (Bson.UTC a) = BS.pack $ TF.formatTime TF.defaultTimeLocale "%Y" a
        extract_time (Bson.UTC a) = BS.pack $ TF.formatTime TF.defaultTimeLocale "%H:%M:%S" a

    userM = case M.lookup "SESSION_KEY" (Req.extract_cookie req) of
        Just key -> DB.run_redis db $ User.redis_get key
        Nothing  -> return Nothing

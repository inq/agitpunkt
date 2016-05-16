{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Main where

import qualified Data.Bson                      as Bson
import qualified Database.MongoDB               as Mongo
import qualified Data.ByteString.Char8          as BS
import qualified Data.Time.Format               as TF
import qualified Data.Map                       as M
import qualified Core.Request                   as Req
import qualified Core.Response                  as Res
import qualified Core.Database                  as DB
import qualified Models.Article                 as Article
import qualified Models.User                    as User
import qualified Core.Html                      as Html

index :: Res.Handler
-- ^ Render the main page
index [] db req = do
    temp <- DB.query db Article.find
    articles <- (mapM read) temp
    putStrLn $ show temp
    user <- userM
    let name = case user of
          Just (User.User {User.name = name}) -> name
          Nothing -> "anonymous"

    return $ Res.success $(Html.parseFile "main/index.html.qh") []
  where
    read :: Bson.Document -> IO [BS.ByteString]
    read document = do
        title <- Mongo.lookup "title" document
        content <- Mongo.lookup "content" document          
        createdAt <- Mongo.lookup "created_at" document
        return
          [ extract title
          , extract content
          , extractDate createdAt
          , extractMonth createdAt
          , extractYear createdAt
          , extractTime createdAt
          ]
      where
        extract (Bson.Bin (Bson.Binary a)) = a
        extractDate (Bson.UTC a) = BS.pack $ TF.formatTime TF.defaultTimeLocale "%d" a
        extractMonth (Bson.UTC a) = BS.pack $ TF.formatTime TF.defaultTimeLocale "%b" a
        extractYear (Bson.UTC a) = BS.pack $ TF.formatTime TF.defaultTimeLocale "%Y" a
        extractTime (Bson.UTC a) = BS.pack $ TF.formatTime TF.defaultTimeLocale "%H:%M:%S" a

    userM = case M.lookup "SESSION_KEY" (Req.extractCookie req) of
        Just key -> DB.runRedis db $ User.redisGet key
        Nothing  -> return Nothing

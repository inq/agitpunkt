{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Handler.Main where

import qualified Data.Bson                      as Bson
import qualified Database.MongoDB               as Mongo
import qualified Data.ByteString.Char8          as BS
import qualified Data.ByteString.Lazy           as LS
import qualified Data.Time.Format               as TF
import qualified Core.Response                  as Res
import qualified Core.Database                  as DB
import qualified Core.Markdown                  as MD
import qualified Models.Article                 as Article
import Core.Html (parse)
import Handler.Application
import Handler.Base

index :: Res.Handler
-- ^ Render the main page
index [] db req = do
    temp <- DB.query db Article.find
    articles <- mapM read temp
    let res = [parse| - foreach articles -> title,content,date,month,year,time
      div { class: "article" }
        div { class: "wrapper" }
            div { class: "label" }
              span { class: "date" }
                = date
              span { class: "month-year" }
                span { class: "month" }
                  = month
                span { class: "year" }
                  = year
              span { class: "time" }
                = time
            div { class: "title" }
              = title
            div { class: "content" }
              = content
      |]
    res' <- layout res [] db req
    return $ Res.success (BS.concat res') []
  where
    read :: Bson.Document -> IO [BS.ByteString]
    read document = do
        title <- Mongo.lookup "title" document
        content <- Mongo.lookup "content" document
        createdAt <- Mongo.lookup "created_at" document
        return
          [ extract title
          , convert content
          , extractDate createdAt
          , extractMonth createdAt
          , extractYear createdAt
          , extractTime createdAt
          ]
      where
        convert content = case MD.convert $ LS.fromStrict $ extract content of
            Just str -> LS.toStrict str
            _ -> "parse error"
        extract (Bson.String a) = a
        extractDate (Bson.UTC a) = BS.pack $ TF.formatTime TF.defaultTimeLocale "%d" a
        extractMonth (Bson.UTC a) = BS.pack $ TF.formatTime TF.defaultTimeLocale "%b" a
        extractYear (Bson.UTC a) = BS.pack $ TF.formatTime TF.defaultTimeLocale "%Y" a
        extractTime (Bson.UTC a) = BS.pack $ TF.formatTime TF.defaultTimeLocale "%H:%M:%S" a

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Handler.Main where

import qualified Data.Bson                      as Bson
import qualified Database.MongoDB               as Mongo
import qualified Data.ByteString.Char8          as BS
import qualified Data.ByteString.Lazy           as LS
import qualified Data.Time.Format               as TF
import qualified Core.Response                  as Res
import qualified Core.Markdown                  as MD
import qualified Models.Article                 as A
import Core.Component (Handler, Component, runDB)
import Core.Html (parse)
import Handler.Application

renderArticle :: A.Article -> Component
-- ^ Render the article
renderArticle (A.Article i t c d) = do
    [parse|div { class: "article" }
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
              = t
            div { class: "content" }
              = c
     |]
  where
    date = TF.formatTime TF.defaultTimeLocale "%d" d
    month = TF.formatTime TF.defaultTimeLocale "%b" d
    year = TF.formatTime TF.defaultTimeLocale "%Y" d
    time = TF.formatTime TF.defaultTimeLocale "%H:%M:%S" d


index :: Handler
-- ^ Render the main page
index = do
    articles <- runDB A.find
    res <- layout [parse| - map articles -> article
        ^ renderArticle article
      |]
    return $ Res.success res []

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Handler.Main where

import qualified Data.Time.Format                 as TF
import qualified Core.Response                    as Res
import qualified Models.Article                   as A
import qualified Data.ByteString.UTF8             as UTF8
import Core.Component (Handler, Component, runDB)
import Core.Html (parse)
import Handler.Application

index :: Handler
-- ^ Render the main page
index = do
    articles <- runDB A.find
    res <- layout [parse|- map articles -> A.Article i t c d
        div { class: "article" }
          div { class: "wrapper" }
            div { class: "label" }
              span { class: "date" }
                $ TF.formatTime TF.defaultTimeLocale "%d" d
              span { class: "month-year" }
                span { class: "month" }
                  $ TF.formatTime TF.defaultTimeLocale "%b" d
                span { class: "year" }
                  $ TF.formatTime TF.defaultTimeLocale "%Y" d
              span { class: "time" }
                $ TF.formatTime TF.defaultTimeLocale "%H:%M:%S" d
            div { class: "title" }
              = t
            div { class: "content" }
              = c
      |]
    return $ Res.success res []

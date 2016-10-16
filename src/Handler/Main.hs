{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Handler.Main where

import qualified Data.Time.Format as TF
import qualified Core.Response as Res
import qualified Models.Article as A
import Data.ByteString.Lazy (toStrict, fromChunks)
import Core.Component (Handler, runDB)
import Misc.Html (parse)
import Misc.Markdown (convert)
import Handler.Application

index :: Handler
-- ^ Render the main page
index = do
    isAdmin <- isUser "gofiri@gmail.com"
    articles <- runDB A.findAll
    res <- layout [parse|- map articles -> A.Article i t c d
      div { class="article" }
        div { class="wrapper" }
          div { class="label" }
            span { class="date" }
              $ TF.formatTime TF.defaultTimeLocale "%d" d
            span { class="month-year" }
              span { class="month" }
                $ TF.formatTime TF.defaultTimeLocale "%b" d
              span { class="year" }
                $ TF.formatTime TF.defaultTimeLocale "%Y" d
            span { class="time" }
              $ TF.formatTime TF.defaultTimeLocale "%H:%M:%S" d
          div { class="title" }
            = t
          div { class="content" }
            = unwrap c
            - if isAdmin
              p
                a { href $ articleUri i }
                  | Edit
     |]
    return $ Res.success res []
  where
    articleUri (Just id') = "/article/edit/" ++ show id'
    articleUri _ = "Unreachable"
    unwrap c = case convert $ fromChunks [c] of
      Just s -> toStrict s
      Nothing -> ""

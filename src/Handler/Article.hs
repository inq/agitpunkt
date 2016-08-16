{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Handler.Article where

import qualified Core.Response                    as Res
import qualified Data.Time.Clock                  as C
import qualified Models.Article                   as Article
import qualified Control.Monad.State              as MS
import qualified Data.ByteString.UTF8             as UTF8
import Core.Component (Handler, Component, runDB, getParams, postData')
import Core.Html (parse)
import Handler.Application

show :: Handler
-- ^ Test parsing URI parameters
show = do
    [category, article, index] <- getParams
    res <- [parse|div
      p
        = category
      p
        = article
      p
        = index
     |]
    return $ Res.success res []

articleForm :: Component
articleForm = [parse|div { class: "content" }
    div
      form { action: "/article/new", method: "post" }
        input { type: "text", name: "title" }
        textarea { name: "content", id: "content-box" }
        input { type: "submit", value: "Submit" }
   |]

new :: Handler
-- ^ Render the formm
new = do
    res <- layout articleForm
    return $ Res.success res []

create :: Handler
-- ^ Create a new article from the given POST data
create = do
    time <- MS.liftIO C.getCurrentTime
    title <- postData' "title"
    content <- postData' "content"
    runDB $ Article.save $ Article.Article Nothing title content time
    res <- layout articleForm
    return $ Res.success res ["HELLO=WORLD"]

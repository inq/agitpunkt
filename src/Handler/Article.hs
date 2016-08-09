{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Handler.Article where

import qualified Data.ByteString.Char8          as BS
import qualified Core.Request                   as Req
import qualified Core.Response                  as Res
import qualified Data.Time.Clock                as C
import qualified Core.Database                  as DB
import qualified Models.Article                 as Article
import Core.Html (parse)
import Handler.Application
import Data.Map ((!))

show :: Res.Handler
-- ^ Test parsing URI parameters
show [category, article, index] db req = do
    res <- [parse|div
      p
        = category
      p
        = article
      p
        = index
     |]
    return $ Res.success (BS.concat res) []

articleForm :: Res.Component
articleForm [] db req =
    [parse|div { class: "content" }
      div
        form { action: "/article/new", method: "post" }
          input { type: "text", name: "title" }
          textarea { name: "content", id: "content-box" }
          input { type: "submit", value: "Submit" }
     |]

new :: Res.Handler
-- ^ Render the formm
new [] db req = do
    let view = articleForm [] db req
    res <- layout view [] db req
    return $ Res.success (BS.concat res) []

create :: Res.Handler
-- ^ Create a new article from the given POST data
create [] db req = do
    time <- C.getCurrentTime
    print req
    DB.query db (Article.save $ Article.Article Nothing title content time)
    let view = articleForm [] db req
    res <- layout view [] db req
    return $ Res.success (BS.concat res) ["HELLO=WORLD"]
  where
    title   = post ! "title"
    content = post ! "content"
    post    = Req.post req

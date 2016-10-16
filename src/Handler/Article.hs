{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Handler.Article where

import qualified Core.Response as Res
import qualified Data.Time.Clock as C
import qualified Models.Article as Article
import qualified Control.Monad.State as MS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Bson as Bson
import Core.Component (Handler, Component, runDB, getParams, postData')
import Misc.Html (parse)
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

edit :: Handler
-- ^ Edit form
edit = do
    [bsid] <- getParams
    let id' = read $ BS.unpack bsid :: Bson.ObjectId
    Just article <- runDB $ Article.find id'
    let createdAt = Prelude.show $ Article.created_at article
        uri = case Article._id article of
          Just o -> BS.concat [ "/article/edit/", BS.pack $ Prelude.show o ]
          _ -> "Unreachable"
    res <- layout [parse|div { class="article" }
      div { class="wrapper" }
        form { action=uri, method="post" }
          input { class="editbox", type="text", name="title", value=Article.title article }
          input { class="editbox", type="text", name="created_at", value$createdAt }
          textarea { class="content", name="content", id="content-box" }
            = Article.content article
          input { type="submit", value="Submit" }
     |]
    return $ Res.success res []

update :: Handler
-- ^ Update
update = do
    [bsid] <- getParams
    let id' = read $ BS.unpack bsid
    title <- postData' "title"
    content <- postData' "content"
    createdAt <- postData' "created_at"

    let article = Article.Article (Just id') title content (read $ BS.unpack createdAt)
    runDB $ Article.update article
    return $ Res.redirect "/" []


new :: Handler
-- ^ Render the formm
new = do
    createdAt <- MS.liftIO $ Prelude.show <$> C.getCurrentTime
    res <- layout [parse|div { class="content" }
      div { class="wrapper" }
        form { action="/article/new", method="post" }
          input { class="editbox", type="text", name="title" }
          input { class="editbox", type="text", name="created_at", value$createdAt }
          textarea { class="content", name="content", id="content-box" }
          input { type="submit", value="Submit" }
     |]
    return $ Res.success res []

create :: Handler
-- ^ Create a new article from the given POST data
create = do
    title <- postData' "title"
    content <- postData' "content"
    createdAt <- postData' "created_at"
    runDB $ Article.save $ Article.Article Nothing title content (read $ BS.unpack createdAt)
    return $ Res.redirect "/" []

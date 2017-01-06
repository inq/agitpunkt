{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Handler.Article
  ( index
  , page
  , view
  , edit
  , new
  , create
  , update
  ) where

import qualified Core.Response as Res
import qualified Data.Time.Clock as C
import qualified Models.Article as Article
import qualified Control.Monad.State as MS
import qualified Data.ByteString.Char8 as BS
import qualified Data.Bson as Bson
import qualified Misc.Markdown as Markdown
import qualified Config
import Data.ByteString.Lazy ( toStrict, fromChunks )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Core.Component ( Handler, runDB, getParams, postData' )
import Misc.Html ( parse )
import Handler.Application

doPage :: Int -> Handler
-- ^ The actual main page renderer
doPage p = do
    isAdmin <- isUser Config.adminUser
    total <- runDB Article.count
    articles <- runDB $ Article.list Config.articlePerPage p
    res <- layout [parse|div
      - map articles -> Article.Article i t c d
        div { class="article" }
          div { class="wrapper" }
            div { class="label" }
              span { class="date" }
                $ formatTime defaultTimeLocale "%d" d
              span { class="month-year" }
                span { class="month" }
                  $ formatTime defaultTimeLocale "%b" d
                span { class="year" }
                  $ formatTime defaultTimeLocale "%Y" d
              span { class="time" }
                $ formatTime defaultTimeLocale "%H:%M:%S" d
            div { class="title" }
              = t
            div { class="content" }
              = unwrap c
              - if isAdmin
                p
                  a { href $ articleUri i }
                    | Edit
      ^ pager p total
     |]
    return $ Res.success res []
  where
    articleUri (Just id') = "/article/edit/" ++ show id'
    articleUri _ = "Unreachable"
    unwrap c = case Markdown.convert $ fromChunks [c] of
      Just s -> toStrict s
      Nothing -> ""

index :: Handler
-- ^ The main page
index = doPage 0

page :: Handler
-- ^ Main page with page argument
page = do
    [num] <- getParams
    doPage $ read $ BS.unpack num


view :: Handler
-- ^ Test parsing URI parameters
view = do
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
    let createdAt = show $ Article.createdAt article
        uri = case Article._id article of
          Just o -> BS.concat [ "/article/edit/", BS.pack $ show o ]
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
    createdAt <- MS.liftIO $ show <$> C.getCurrentTime
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

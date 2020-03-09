{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.Article
  ( indexH
  , page
  , readArticle
  , edit
  , new
  , view
  , create
  , update
  ) where

import           App.Component       (Handler, getParams, postData', runDB)
import qualified Config
import qualified Control.Monad.State as MS
import qualified Core.Response       as Res
import qualified Data.Bson           as Bson
import qualified Data.Text           as Text
import           Data.Text.Lazy      (fromChunks, toStrict)
import qualified Data.Time.Clock     as C
import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           Handler.Application
import           Misc.Html           (parse)
import qualified Misc.Markdown       as Markdown
import qualified Models.Article      as Article

doPage :: Int -> Handler
-- ^ The actual main page renderer
doPage p = do
  isAdmin <- isUser Config.adminUser
  total <- runDB Article.count
  articles <- runDB $ Article.list Config.articlePerPage p
  res <- layout [parse|div
      - map articles -> Article.Article i u t _c d
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
              a { href $ showUri u }
                = t
            - if isAdmin
              p
                a { href $ editUri i }
                  | Edit
      ^ pager p total
     |]
  return $ Res.success res []
  where
    editUri (Just id') = "/article/edit/" ++ show id'
    editUri _          = "Unreachable"
    showUri u' = "/read/" ++ Text.unpack u'
    _unwrap c = maybe "" toStrict (Markdown.convert $ fromChunks [c])

readArticle :: Handler
-- ^ The actual main page renderer
readArticle = do
  [u'] <- getParams
  isAdmin <- isUser Config.adminUser
  articles <- runDB $ Article.fromUri u'
  res <- layout [parse|div
      - map articles -> Article.Article i _u t c d
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
    articleUri _          = "Unreachable"
    unwrap c =
      maybe "" toStrict (Markdown.convert $ fromChunks [c])

indexH :: Handler
-- ^ The main page
indexH = doPage 0

page :: Handler
-- ^ Main page with page argument
page = do
  [num] <- getParams
  doPage $ read $ Text.unpack num

view :: Handler
-- ^ Test parsing URI parameters
view = do
  [category, article, index] <- getParams
  res <-
    [parse|div
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
  let id' = read $ Text.unpack bsid :: Bson.ObjectId
  Just article <- runDB $ Article.find id'
  let createdAt = show $ Article.createdAt article
      uri =
        case Article._id article of
          Just o -> Text.concat ["/article/edit/", Text.pack $ show o]
          _      -> "Unreachable"
  res <-
    layout
      [parse|div { class="article" }
      div { class="wrapper" }
        form { action=uri, method="post" }
          input { class="editbox", type="text", name="uri", value=Article.uri article }
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
  let id' = read $ Text.unpack bsid
  uri <- postData' "uri"
  title <- postData' "title"
  content <- postData' "content"
  createdAt <- postData' "created_at"
  let article =
        Article.Article (Just id') uri title content (read $ Text.unpack createdAt)
  runDB $ Article.update article
  return $ Res.redirect "/" []

new :: Handler
-- ^ Render the formm
new = do
  createdAt <- MS.liftIO $ show <$> C.getCurrentTime
  res <-
    layout
      [parse|div { class="content" }
      div { class="wrapper" }
        form { action="/article/new", method="post" }
          input { class="editbox", type="text", name="uri" }
          input { class="editbox", type="text", name="title" }
          input { class="editbox", type="text", name="created_at", value$createdAt }
          textarea { class="content", name="content", id="content-box" }
          input { type="submit", value="Submit" }
     |]
  return $ Res.success res []

create :: Handler
-- ^ Create a new article from the given POST data
create = do
  uri <- postData' "uri"
  title <- postData' "title"
  content <- postData' "content"
  createdAt <- postData' "created_at"
  runDB $
    Article.save $
    Article.Article Nothing uri title content (read $ Text.unpack createdAt)
  return $ Res.redirect "/" []

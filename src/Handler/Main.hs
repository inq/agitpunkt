{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Main where

import qualified Data.ByteString.Char8          as BS
import qualified Manicure.Route                 as Route
import qualified Manicure.Request               as Req
import qualified Manicure.Response              as Res
import qualified Manicure.Database              as DB
import qualified Database.MongoDB               as M
import qualified Data.Text                      as T
import qualified Manicure.Html                  as Html
import qualified Models.Article                 as Article
import qualified Data.Time.Clock                as C
import qualified Data.Bson                      as Bson
import Data.Map ((!))

article :: Res.Handler
-- ^ Test parsing URI parameters
article [category, article, index] db req = do
    return $ Res.success (head $(Html.parseFile "Views/article.html.qh")) []

new_article :: Res.Handler
-- ^ Create a new article from the given POST data
new_article [] db req = do
    time <- C.getCurrentTime
    DB.query db (Article.save $ Article.Article Nothing title content time)
    return $ Res.success (head $(Html.parseFile "Views/new_article.html.qh")) ["HELLO=WORLD"]
  where
    title   = post ! "title"
    content = post ! "content"
    post    = Req.post req

index :: Res.Handler
-- ^ Render the main page
index [] db req = do
    articles <- DB.query db DB.find
    titles <- extract articles "title"
    let cookie = [Req.extract_cookie req]
    return $ Res.success (head $(Html.parseFile "Views/index.html.qh")) []
  where
    extract documents key = mapM read documents :: IO [BS.ByteString]
      where
        read document = do
            res <- M.lookup key document
            return $ extract res 
          where
            extract (Bson.Binary a) = a

test :: Res.Handler
-- ^ Render the test page
test [] db teq = do
    return $ Res.success (head $(Html.parseFile "Views/test.html.qh")) []

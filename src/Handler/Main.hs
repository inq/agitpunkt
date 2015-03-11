{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Main where

import qualified Data.ByteString.Char8          as BS
import qualified Manicure.Auth                  as Auth
import qualified Manicure.Route                 as Route
import qualified Manicure.Request               as Req
import qualified Manicure.Response              as Res
import qualified Manicure.Database              as DB
import qualified Database.MongoDB               as M
import qualified Manicure.ByteString            as ByteString
import qualified Data.Text                      as T
import qualified Manicure.Html                  as Html
import qualified Models.Article                 as Article
import qualified Data.Time.Clock                as C
import qualified Data.Bson                      as Bson
import qualified Network.Curl                   as Curl

import Control.Monad
import Data.Map ((!))

article :: Res.Handler
-- ^ Test parsing URI parameters
article [category, article, index] db req = do
    return $ Res.success (head $(Html.parseFile "Views/article.html.qh")) []

signin :: Res.Handler
-- ^ Sign in page
signin [] db req = do
    return $ Res.redirect $ Auth.oauth_url "https://whitesky.net/step2"

step2 :: Res.Handler
-- ^ OAuth2 step 2
step2 [] db req = do
    query_str <- liftM snd $ Curl.curlGetString (BS.unpack $ Auth.access_token_url "https://whitesky.net/step2" code) []
    let query = ((ByteString.split_and_decode . BS.pack) query_str) ! "access_token"
    return $ Res.success (head $(Html.parseFile "Views/test.html.qh")) []
  where
    code = Req.query_str req ! "code"

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

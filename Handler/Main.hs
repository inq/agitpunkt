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
import Data.Map ((!))

article :: Res.Handler
article [category, article, index] db req = do
    return $ Res.success $ head $(Html.parseFile "Views/article.html.qh") 

new_article :: Res.Handler
new_article [] db req = do
    return $ Res.success $ head $(Html.parseFile "Views/new_article.html.qh")     
  where
    title   = post ! "title"
    content = post ! "content"
    post    = Req.post req

index :: Res.Handler
index [] db req = do
    articles <- DB.query db DB.find
    titles <- extract articles "title"
    return $ Res.success $ head $(Html.parseFile "Views/index.html.qh") 
  where
    extract :: [M.Document] -> T.Text -> IO [String]
    extract documents key = mapM read documents
      where
        read document = M.lookup key document

test :: Res.Handler
test [] db teq = do
    return $ Res.success $ head $(Html.parseFile "Views/test.html.qh") 

post_test :: Res.Handler
post_test [] db val = do
    return $ Res.success "post test"

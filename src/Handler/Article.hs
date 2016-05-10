{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Article where

import qualified Core.Request                   as Req
import qualified Core.Response                  as Res
import qualified Core.Html                      as Html                
import qualified Data.Time.Clock                as C
import qualified Core.Database                  as DB
import qualified Models.Article                 as Article
import Data.Map ((!))

show :: Res.Handler
-- ^ Test parsing URI parameters
show [category, article, index] db req = do
    return $ Res.success $(Html.parseFile "article/show.html.qh") []

new :: Res.Handler
-- ^ Render the formm
new [] db req = do
    return $ Res.success $(Html.parseFile "article/new.html.qh") []

create :: Res.Handler
-- ^ Create a new article from the given POST data
create [] db req = do
    time <- C.getCurrentTime
    DB.query db (Article.save $ Article.Article Nothing title content time)
    return $ Res.success $(Html.parseFile "article/new.html.qh") ["HELLO=WORLD"]
  where
    title   = post ! "title"
    content = post ! "content"
    post    = Req.post req

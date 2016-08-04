{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Application where

import qualified Data.ByteString.Char8          as BS
import qualified Core.Database                  as DB
import qualified Core.Response                  as Res
import qualified Core.Request                   as Req
import qualified Models.Category                as Category
import qualified Models.User                    as User
import qualified Data.Map                       as M
import Core.Html (parse)
import Handler.Base

layout :: BS.ByteString -> Res.Component
layout yield [] db req = do
    categories <- (toStrList . convert . reverse) <$> DB.query db Category.find
    user <- userM
    let (name, login) = case user of
          Just User.User {User.name = name} -> (name, True)
          Nothing -> ("anonymous", False)
    return [parse|html
        head
          meta   { charset: "UTF-8" }
          link   { href: "/static/q.css", rel: "stylesheet" }
        body
          div { id: "header" }
            div { class: "wrapper" }
              div { id: "signin-box" }
                - render partial/login-box.qh
              p
                span { class: "red" }
                  | {
                | inkyu.kr
                span { class: "red" }
                  | }
          div { id: "menu" }
            ul
              - foreach categories -> name,id,lvl
                li { class: lvl, data-id: id }
                  = name
          = yield
          div  { id: "footer" }
            div { id: "footer-left" }
              p { id: "compiled-at" }
                = compiled
              p { id: "author" }
                | Inkyu Lee
            div { id: "footer-right" }
              a { href: "https://github.com/inq/agitpunkt" }
                - render partial/github.qh
     |]
  where
    userM = case M.lookup "SESSION_KEY" (Req.extractCookie req) of
        Just key -> DB.runRedis db $ User.redisGet key
        Nothing  -> return Nothing

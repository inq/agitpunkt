{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Handler.Application where

import qualified Data.ByteString.Char8 as BS
import qualified Models.User as User
import qualified Config
import Core.Session (query)
import Core.Component (Component, FlexComp, getCookie, getSessionStore)
import Control.Monad.State (liftIO)
import Control.Monad (unless)
import Misc.Html (parse)
import Handler.Base

pager :: Int -> Int -> Component
pager current total =
    [parse|div { class="wrapper pager" }
      - map pages -> page
        a { href=buildUri page, class=buildClass page }
          $ show page
    |]
  where
    buildClass p = if p == current
      then BS.pack "current"
      else BS.pack "non-current"
    buildUri p = BS.pack ("/page/" ++ show p)
    pages = [0..(totalPage - 1)]
    totalPage = quot (total + Config.articlePerPage - 1) Config.articlePerPage

isUser :: BS.ByteString -> FlexComp Bool
isUser email = do
    session_key <- getCookie "SESSION_KEY"
    u <- case session_key of
      Just key -> do
        -- TODO: Need to be shorten.
        ss <- getSessionStore
        liftIO $ query key ss
      Nothing -> return Nothing
    return $ case u of
      Just User.User {User.email = email'}
        | email == email' -> True
      _ -> False

assertUser :: BS.ByteString -> FlexComp ()
assertUser email = do
  res <- isUser email
  unless res $ fail "Assertion failed"

github :: Component
github = [parse|svg { xmlns="http://www.w3.org/2000/svg", viewBox="0 0 48 48", class="github-icon" }
    path { class="st0", d="M23.928 1.15C11 1.15.514 11.638.514 24.566c0 10.343 6.75 19.105 15.945 22.265 1.148.144 1.58-.574 1.58-1.15v-4.02c-6.465 1.436-7.902-3.16-7.902-3.16-1.005-2.73-2.586-3.45-2.586-3.45-2.154-1.435.144-1.435.144-1.435 2.298.144 3.59 2.442 3.59 2.442 2.156 3.59 5.46 2.586 6.753 2.01.142-1.58.86-2.585 1.435-3.16-5.17-.574-10.63-2.585-10.63-11.635 0-2.585.862-4.596 2.442-6.32-.287-.575-1.005-3.017.288-6.177 0 0 2.01-.574 6.464 2.442 1.866-.574 3.877-.718 5.888-.718 2.01 0 4.022.286 5.89.717 4.453-3.016 6.464-2.442 6.464-2.442 1.293 3.16.43 5.602.287 6.177a9.29 9.29 0 0 1 2.44 6.32c0 9.05-5.458 10.918-10.63 11.492.863.718 1.58 2.155 1.58 4.31v6.464c0 .574.432 1.292 1.58 1.15 9.338-3.16 15.946-11.924 15.946-22.266-.143-12.785-10.63-23.27-23.558-23.27z", clip-rule="evenodd", fill="#191717", fill-rule="evenodd" }
   |]

loginbox :: Component
loginbox = do
    session_key <- getCookie "SESSION_KEY"
    u <- case session_key of
        Just key -> do
          -- TODO: Need to be shorten.
          ss <- getSessionStore
          liftIO $ query key ss
        Nothing -> return Nothing
    let (n, l) = case u of
          Just User.User {User.name = name'} -> (name', True)
          Nothing -> ("anonymous", False)
    [parse|span { class="signin-box" }
      - if l
        span { class="name-box" }
          = n
        a { href="/auth/signout" }
          | Sign out
        a { href="/article/new" }
          | new article
      - if not l
        a { href="/auth/signin" }
          | sign in
     |]

layout :: Component -> Component
layout yield =
    [parse|html
        head
          meta { charset="UTF-8" }
          link { href="/static/q.css", rel="stylesheet" }
        body
          div { id="header" }
            div { class="wrapper" }
              div { id="signin-box" }
                ^ loginbox
              p
                span { class="red" }
                  | {
                | inkyu.kr
                span { class="red" }
                  | }
          div { id="menu" }
            ul
          ^ yield
          div  { id="footer" }
            div { id="footer-left" }
              p { id="compiled-at" }
                = compiled
              p { id="author" }
                | Inkyu Lee
            div { id="footer-right" }
              a { href="https://github.com/inq/agitpunkt" }
                ^ github
     |]

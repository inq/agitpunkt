{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Handler.Auth where

import qualified Core.Response as Res
import qualified Data.ByteString.Char8 as BS
import qualified Models.User as User
import qualified Control.Monad.State as MS
import App.Session (storeSession)
import App.Component (Component, Handler, getSessionStore, getUserStore, postData')
import Control.Monad.State (liftIO)
import Misc.Crypto (hashPassword, generateKey)
import Misc.Html (parse)
import Handler.Application

signupForm :: Component
signupForm = [parse|div { class="wrapper" }
  form { action="/auth/signup", method="post" }
    | email
    input { type="text", name="email" }
    | password
    input { type="password", name="password" }
    | name
    input { type="name", name="name" }
    input { type="submit" }
 |]

destroy :: Handler
-- ^ Render the form
destroy =
    return $ Res.redirect "/" ["SESSION_KEY="]

index :: Handler
-- ^ Render the signin form
index = do
  html <- layout [parse|div { class="article" }
    div { class="wrapper" }
      form { action="/auth/signin", method="post" }
        | email
        input { type="text", name="email" }
        | password
        input { type="password", name="password" }
        input { type="submit" }
  |]
  return $ Res.success html []

signin :: Handler
-- ^ Sign in and redirect to home
signin = do
    email <- postData' "email"
    password <- hashPassword <$> postData' "password"
    us <- getUserStore
    user <- liftIO $ User.signIn us email password
    cookies <- case user of
        Just a -> do
            -- TODO: Need to be shorten.
            key <- MS.liftIO generateKey
            ss <- getSessionStore
            liftIO $ storeSession key a ss
            return [BS.concat ["SESSION_KEY=", key]]
        Nothing -> return []
    return $ Res.redirect "/" cookies

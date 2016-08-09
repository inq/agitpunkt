{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module Handler.Auth where

import qualified Core.Response                    as Res
import qualified Core.Session                     as Ses
import qualified Data.Time.Clock                  as C
import qualified Data.ByteString.Char8            as BS
import qualified Models.User                      as User
import qualified Control.Monad.State              as MS
import Core.Component (Handler, runDB, runRedis, postData')
import Core.Html (parse)

signupForm :: IO [BS.ByteString]
signupForm = [parse|form { action: "/auth/signup", method: "post" }
    | email
    input { type: "text", name: "email" }
    | password
    input { type: "password", name: "password" }
    | name
    input { type: "name", name: "name" }
    input { type: "submit" }
   |]

new :: Handler
-- ^ Render the form
new = do
    error "prevented!"
    form <- MS.liftIO signupForm
    return $ Res.success (BS.concat form) []

destroy :: Handler
-- ^ Render the form
destroy =
    return $ Res.redirect "/" ["SESSION_KEY="]

index :: Handler
-- ^ Render the signin form
index = do
    html <- [parse|form { action: "/auth/signin", method: "post" }
      | email
      input { type: "text", name: "email" }
      | password
      input { type: "password", name: "password" }
      input { type: "submit" }
     |]
    return $ Res.success (BS.concat html) []

signin :: Handler
-- ^ Sign in and redirect to home
signin = do
    email <- postData' "email"
    password <- User.hashPassword <$> postData' "password"
    user <- runDB $ User.signIn email password
    cookies <- case user of
        Just a -> do
            key <- MS.liftIO Ses.generateKey
            _ <- runRedis $ User.redisHash key a
            return [BS.concat ["SESSION_KEY=", key]]
        Nothing -> return []
    return $ Res.redirect "/" cookies

create :: Handler
-- ^ Create a new article from the given POST data
create = do
    error "prevented!"
    time <- MS.liftIO C.getCurrentTime
    name <- postData' "name"
    email <- postData' "email"
    password <- User.hashPassword <$> postData' "password"
    runDB $ User.save $ User.User
      { User._id = Nothing
      , User.email = email
      , User.name = name
      , User.password = Just password
      , User.createdAt = Nothing }
    html <- MS.liftIO signupForm
    return $ Res.success (BS.concat html) ["HELLO=WORLD"]

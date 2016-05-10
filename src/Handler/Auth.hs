{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Auth where

import qualified Core.Request                     as Req
import qualified Core.Response                    as Res
import qualified Core.Html                        as Html                
import qualified Core.Session                     as Ses
import qualified Core.Database                    as DB
import qualified Data.Time.Clock                  as C
import qualified Data.ByteString.Char8            as BS
import qualified Models.User                      as User

import Data.Map ((!))

new :: Res.Handler
-- ^ Render the form
new [] db req = do
    error "prevented!"
    return $ Res.success $(Html.parseFile "auth/signup.html.qh") []

index :: Res.Handler
-- ^ Render the signin form
index [] db req = do
    return $ Res.success $(Html.parseFile "auth/signin.html.qh") []

signin :: Res.Handler
-- ^ Sign in and redirect to home
signin [] db req = do
    user <- DB.query db (User.signIn email password)
    case user of
        Just a -> do
            key <- Ses.generateKey
            putStrLn $ show a
            DB.runRedis db $ User.redisHash key a
            return $ Res.success $(Html.parseFile "auth/signin.html.qh") [BS.concat ["SESSION_KEY=", key]]
        Nothing ->
            return $ Res.redirect "/" []
  where
    email = post ! "email"
    password = User.hashPassword (post ! "password")
    post = Req.post req

create :: Res.Handler
-- ^ Create a new article from the given POST data
create [] db req = do
    error "prevented!"
    time <- C.getCurrentTime
    DB.query db (User.save $ User.User 
      { User._id = Nothing
      , User.email = email
      , User.name = name
      , User.password = Just password
      , User.createdAt = Nothing } )
    return $ Res.success $(Html.parseFile "auth/signup.html.qh") ["HELLO=WORLD"]
  where
    name = post ! "name"
    email = post ! "email"
    password = User.hashPassword (post ! "password")
    post = Req.post req
 

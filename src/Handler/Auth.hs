{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Auth     
    ( signin
    ) where

import qualified Data.ByteString.Char8          as BS
import qualified Manicure.Auth                  as Auth
import qualified Manicure.Request               as Req
import qualified Manicure.Response              as Res
import qualified Manicure.Database              as DB
import qualified Manicure.Session               as Session
import qualified Manicure.ByteString            as ByteString
import qualified Manicure.Html                  as Html
import qualified Manicure.Http                  as Http
import qualified Data.Map                       as M
import qualified Models.User                    as User
import qualified Models.Article                 as Article
import Control.Monad
import Data.Map ((!))

signin :: Res.Handler
-- ^ Sign in page
signin [] db req = response
  where
    redirectUri = "https://inkyu.kr/signin"
    response = case M.lookup "code" $ Req.queryStr req of
        Just code -> do 
            queryStr <- Http.fetch $ Auth.accessTokenUrl redirectUri code
            userInfo <- case M.lookup "access_token" $ ByteString.splitAndDecode '&' queryStr of
                Just token -> Http.fetch $ Auth.facebookMeUrl token
                Nothing    -> return ""
            let user = User.fromJson userInfo
            DB.query db $ User.upsert user
            sessionKey <- Session.generateKey
            DB.runRedis db $ User.redisHash sessionKey user
            let query = show user
            return $ Res.success $(Html.parseFile "auth/test.html.qh") [
                BS.concat ["SESSION_KEY=", sessionKey]
              ]
        Nothing   -> do
            return $ Res.redirect $ Auth.oauthUrl redirectUri

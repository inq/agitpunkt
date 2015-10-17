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
    redirect_uri = "https://inkyu.kr/signin"
    response = case M.lookup "code" $ Req.query_str req of
        Just code -> do 
            query_str <- Http.fetch $ Auth.access_token_url redirect_uri code
            user_info <- case M.lookup "access_token" $ ByteString.split_and_decode '&' query_str of
                Just token -> Http.fetch $ Auth.facebook_me_url token
                Nothing    -> return ""
            let user = User.from_json user_info
            DB.query db $ User.upsert user
            session_key <- Session.generateKey
            DB.run_redis db $ User.redis_hash session_key user
            let query = show user
            return $ Res.success $(Html.parseFile "auth/test.html.qh") [
                BS.concat ["SESSION_KEY=", session_key]
              ]
        Nothing   -> do
            return $ Res.redirect $ Auth.oauth_url redirect_uri

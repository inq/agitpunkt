{-# LANGUAGE OverloadedStrings #-}

module App.Component where

import           App.Session          (SessionStore)
import           Control.Monad.State  (StateT, gets, liftIO, runStateT)
import qualified Core.Database        as DB
import qualified Core.Request         as Req
import qualified Core.Request.Content as Content
import qualified Core.Response        as Res
import           Data.List            (find)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Database.MongoDB     as Mongo
import           Models.User          (UserStore)

-- * Data types
data ResState = ResState
  { conn     :: DB.Connection
  , params   :: [Text]
  , req      :: Req.Request
  , sessions :: SessionStore
  , users    :: UserStore
  }

-- * Type Aliases
type Handler = StateT ResState IO Res.Response

type Component = StateT ResState IO Text

type FlexComp a = StateT ResState IO a

-- * Handler
runHandler
  :: Handler
  -> [Text]
  -> DB.Connection
  -> Req.Request
  -> SessionStore
  -> UserStore
  -> IO (Res.Response, ResState)
runHandler c p n r s u = runStateT c (ResState n p r s u)

runDB :: Mongo.Action IO a -> StateT ResState IO a
-- ^ Run the DB action
runDB a = do
  n <- gets conn
  liftIO $ DB.query n a

-- | TODO: Clean up
getUserStore :: StateT ResState IO UserStore
-- ^ Pass the user store
getUserStore = gets users

getSessionStore :: StateT ResState IO SessionStore
-- ^ Pass the session store
getSessionStore = gets sessions

getCookie :: Text -> StateT ResState IO (Maybe Text)
-- ^ Read cookie from the state
getCookie key = gets $ M.lookup key . Req.extractCookie . req

getParams :: StateT ResState IO [Text]
-- ^ Get parameters
getParams = gets params

postData :: Text -> StateT ResState IO (Maybe Content.Context)
-- ^ Read Post variable
postData key = gets $ M.lookup key . Req.content . req

postData' :: Text -> StateT ResState IO Text
-- ^ Read Post variable
postData' key = do
  res <- gets $ M.lookup key . Req.content . req
  return $
    case res of
      Just (Content.MkText bs) -> bs
      _                        -> ""

requestHeader :: Text -> StateT ResState IO (Maybe Text)
-- ^ Read request header
requestHeader key = do
  headers <- gets $ Req.headers . req
  return (snd <$> find sel headers)
  where
    sel (k, _) = k == key

reqm :: StateT ResState IO Text
-- ^ Read request header
reqm = do
  headers <- gets $ Req.headers . req
  return $ Text.pack $ show headers

reqn :: StateT ResState IO Text
-- ^ Read request header
reqn = do
  headers <- gets $ Req.content . req
  return $ Text.pack $ show headers

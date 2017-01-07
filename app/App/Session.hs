{-# LANGUAGE OverloadedStrings #-}
module App.Session where

import qualified Data.ByteString.Char8 as BS
import qualified Models.User as User
import qualified Data.Map as M
import Misc.Crypto (generateKey)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, takeTMVar, putTMVar, readTMVar)

type SessionStore = (TMVar (M.Map BS.ByteString User.User))

initStore :: IO SessionStore
-- ^ Initialize the session store.
initStore = atomically $ newTMVar (M.fromList [])

storeSession :: BS.ByteString -> User.User -> SessionStore -> IO ()
-- ^ Insert a new session.
storeSession key value store = atomically $ do
  s <- takeTMVar store
  putTMVar store $ M.insert key value s

querySession :: BS.ByteString -> SessionStore -> IO (Maybe User.User)
-- ^ Find the user and return it.
querySession key store = atomically $ do
  s <- readTMVar store
  return $ M.lookup key s

mkSession :: IO BS.ByteString
-- ^ Make a new session data
mkSession = generateKey

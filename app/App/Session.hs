{-# LANGUAGE OverloadedStrings #-}
module App.Session where

import qualified Data.ByteString.Char8 as BS
import qualified Models.User as User
import qualified Data.Map as M
import Misc.Crypto (generateKey)
import GHC.Conc.Sync (atomically, TVar, newTVar, readTVar, writeTVar)

type SessionStore = (TVar (M.Map BS.ByteString User.User))

initStore :: IO SessionStore
-- ^ Initialize the session store.
initStore = atomically $ newTVar (M.fromList [])

storeSession :: BS.ByteString -> User.User -> SessionStore -> IO ()
-- ^ Insert a new session.
storeSession key value store = atomically $ do
  s <- readTVar store
  writeTVar store $ M.insert key value s

querySession :: BS.ByteString -> SessionStore -> IO (Maybe User.User)
-- ^ Find the user and return it.
querySession key store = atomically $ do
  s <- readTVar store
  return $ M.lookup key s

mkSession :: IO BS.ByteString
-- ^ Make a new session data
mkSession = generateKey

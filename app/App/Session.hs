{-# LANGUAGE OverloadedStrings #-}

module App.Session where

import qualified Data.Map      as M
import           Data.Text     (Text)
import           GHC.Conc.Sync (TVar, atomically, newTVar, readTVar, writeTVar)
import           Misc.Crypto   (generateKey)
import qualified Models.User   as User

type SessionStore = (TVar (M.Map Text User.User))

initStore :: IO SessionStore
-- ^ Initialize the session store.
initStore = atomically $ newTVar (M.fromList [])

storeSession :: Text -> User.User -> SessionStore -> IO ()
-- ^ Insert a new session.
storeSession key value store =
  atomically $ do
    s <- readTVar store
    writeTVar store $ M.insert key value s

querySession :: Text -> SessionStore -> IO (Maybe User.User)
-- ^ Find the user and return it.
querySession key store =
  atomically $ do
    s <- readTVar store
    return $ M.lookup key s

mkSession :: IO Text
-- ^ Make a new session data
mkSession = generateKey

{-# LANGUAGE OverloadedStrings #-}
module Core.Session where

import qualified Crypto.Hash.SHA256               as SHA256
import qualified Data.Time.Clock.POSIX            as POSIX
import qualified Data.ByteString.Char8            as BS
import qualified Core.Database                    as DB
import qualified Foreign.Ptr                      as FP
import qualified Foreign.Storable                 as FS
import qualified Data.ByteString.Unsafe           as BSU
import qualified Data.ByteString.Internal         as BSI
import qualified Data.Bits                        as B
import qualified Models.User as User
import qualified  Data.Map as M
import Control.Monad.STM (STM, atomically)
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, takeTMVar, putTMVar, readTMVar)
import Data.Bits ((.&.))

type SessionStore = (TMVar (M.Map BS.ByteString User.User))

initStore :: IO SessionStore
-- ^ Initialize the session store.
initStore = atomically $ newTMVar (M.fromList [])

insert :: SessionStore -> BS.ByteString -> User.User -> STM ()
-- ^ Insert a new session.
insert store key value = do
  map <- takeTMVar store
  putTMVar store $ M.insert key value map

get :: SessionStore -> BS.ByteString -> STM (Maybe User.User)
-- ^ Find the user and return it.
get store key = do
  map <- readTMVar store
  return $ M.lookup key map

generateKey :: IO BS.ByteString
-- ^ Generate a session key
generateKey = do
    t <- POSIX.getPOSIXTime
    return $ toHex . SHA256.hash . BS.pack $ show t
  where
    hexDigest d
        | d < 10 = d + 48
        | otherwise = d + 87
    toHex bs = BSI.unsafeCreate nl $ go 0
      where
        len = BS.length bs
        nl = 2 * len
        go i p
          | i == len  = return ()
          | otherwise = case BSU.unsafeIndex bs i of
              w -> do
                  FS.poke p (hexDigest $ w `B.shiftR` 4)
                  FS.poke (p `FP.plusPtr` 1) (hexDigest $ w .&. 0xF)
                  go (i + 1) (p `FP.plusPtr` 2)

mkSession :: IO BS.ByteString
-- ^ Make a new session data
mkSession = generateKey

readSession :: DB.Connection -> BS.ByteString -> IO BS.ByteString
readSession = DB.redisGet

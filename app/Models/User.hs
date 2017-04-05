{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Models.User where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Misc.Parser as P
import GHC.Conc.Sync (atomically, TVar, newTVar, readTVar)

-- * Data types

data User = User
  { _id       :: Int
  , email     :: BS.ByteString
  , name      :: BS.ByteString
  , password  :: BS.ByteString
  } deriving (Show)

type UserStore = (TVar (M.Map BS.ByteString User))

parse :: BS.ByteString -> Maybe (M.Map BS.ByteString User)
-- ^ Parse the given bytestring
parse str = case P.parseOnly parseUserList str of
  Right val -> Just val
  _ -> Nothing

parseUserList :: P.Parser (M.Map BS.ByteString User)
-- ^ The actual parser
parseUserList = do
  users <- P.many1 parseUser
  return $ M.fromList $ map (\u -> (email u, u)) users
 where
  parseUser = do
    _id' <- P.decimal <* P.char '\t'
    email' <- P.noneOf1 "\t" <* P.char '\t'
    name' <- P.noneOf1 "\t" <* P.char '\t'
    password' <- P.noneOf1 "\n" <* P.many (P.char '\n')
    return $ User _id' email' name' password'

putUserStore :: UserStore -> IO ()
-- ^ Try to signin
putUserStore store' = do
  map' <- atomically $ readTVar store'
  putStrLn $ show map'

loadUserStore :: FilePath -> IO (Maybe UserStore)
-- ^ Read the TSV file
loadUserStore fileName = do
  res <- BS.readFile fileName
  case parse res of
    Just store -> Just <$> (atomically $ newTVar store)
    _ -> return Nothing

signIn :: UserStore -> BS.ByteString -> BS.ByteString -> IO (Maybe User)
-- ^ Try to signin
signIn store' email' password' = do
  map' <- atomically $ readTVar store'
  return $ case M.lookup email' map' of
    Just user -> if (password user) == password'
      then Just user
      else Nothing
    _ -> Nothing

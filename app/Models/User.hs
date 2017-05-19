{-# LANGUAGE OverloadedStrings #-}

module Models.User where

import           Control.Exception (catch)
import qualified Data.Map          as M
import           Data.Text         (Text)
import qualified Data.Text.IO      as TextIO
import           GHC.Conc.Sync     (TVar, atomically, newTVar, readTVar)
import qualified Misc.Parser       as P

-- * Data types
data User = User
  { _id      :: Int
  , email    :: Text
  , name     :: Text
  , password :: Text
  } deriving (Show)

type UserStore = (TVar (M.Map Text User))

parse :: Text -> Maybe (M.Map Text User)
-- ^ Parse the given bytestring
parse str =
  case P.parseOnly parseUserList str of
    Right val -> Just val
    _         -> Nothing

parseUserList :: P.Parser (M.Map Text User)
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
  print map'

catchError :: IOError -> IO (Maybe a)
-- ^ TODO: Wrap this
catchError = const (pure Nothing)

loadUserStore :: FilePath -> IO (Maybe UserStore)
-- ^ Read the TSV file
loadUserStore fileName = do
  users <- (parse <$> TextIO.readFile fileName) `catch` catchError
  (atomically . newTVar) `mapM` users

signIn :: UserStore -> Text -> Text -> IO (Maybe User)
-- ^ Try to signin
signIn store' email' password' = do
  map' <- atomically $ readTVar store'
  return $
    case M.lookup email' map' of
      Just user ->
        if password user == password'
          then Just user
          else Nothing
      _ -> Nothing

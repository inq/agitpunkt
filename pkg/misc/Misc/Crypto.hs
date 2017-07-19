module Misc.Crypto
  ( hashPassword
  , generateKey
  ) where

import           Data.Digest.Pure.SHA    (sha256, showDigest)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Lazy          (fromStrict)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Time.Clock.POSIX   (getPOSIXTime)

generateKey :: IO Text
-- ^ Generate a session key
generateKey = do
  t <- getPOSIXTime
  return $ hashPassword $ Text.pack $ show t

hashPassword :: Text -> Text
-- ^ Hash the given password
hashPassword = Text.pack . showDigest . sha256 . encodeUtf8 . fromStrict

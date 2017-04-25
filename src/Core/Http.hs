module Core.Http where

import           Control.Lens            ((^.))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as L
import           Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Network.Wreq            as Wreq

data Version = Version
  { major :: {-# UNPACK #-}!Int
  , minor :: {-# UNPACK #-}!Int
  } deriving (Show)

fetch :: Text -> IO Text
fetch url = do
  response <- Wreq.get $ Text.unpack url
  return $ L.toStrict $ decodeUtf8 (response ^. Wreq.responseBody)

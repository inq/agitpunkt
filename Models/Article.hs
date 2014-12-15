module Models.Article where

import qualified Data.ByteString.Char8          as BS
import qualified Data.Time.Clock                as TC

data Article = Article {
  title      :: BS.ByteString,
  content    :: BS.ByteString,
  created_at :: TC.UTCTime
}

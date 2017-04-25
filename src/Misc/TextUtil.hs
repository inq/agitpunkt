{-# LANGUAGE FlexibleInstances #-}

module Misc.TextUtil where

import qualified Data.Map               as M
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy         as L
import qualified Misc.Parser            as P
import           Network.HTTP.Types.URI (urlDecode)

-- * Instances
class StringFamily a where
  convert :: a -> Text

instance StringFamily Text where
  convert = id

instance StringFamily String where
  convert = Text.pack

instance StringFamily L.Text where
  convert = L.toStrict

type QueryString = M.Map Text Text

parse :: Char -> P.Parser [(Text, Text)]
-- ^ Parser for pair
parse splitter = P.sepBy parsePair (P.char splitter)
  where
    spaces = P.skipWhile P.isHorizontalSpace
    parsePair :: P.Parser (Text, Text)
    parsePair = do
      key <- decode <$> (spaces *> P.noneOf1 " =" <* spaces <* P.char '=')
      value <- decode <$> (spaces *> P.noneOf1 (splitter : " ") <* spaces)
      return (key, value)
    decode text = decodeUtf8 (urlDecode True $ encodeUtf8 text)

splitAndDecode :: Char -> Text -> QueryString
-- ^ Split the given string and construct the Map
splitAndDecode mark bs =
  case P.parseOnly (parse mark) bs of
    Right val -> M.fromList val
    Left _    -> M.empty

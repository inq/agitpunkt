{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Misc.TextUtil where

import qualified Data.Map               as M
import           Data.ByteString        (ByteString)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy         as L
import qualified Misc.Parser            as P
import qualified Misc.Parser.ByteString as BSParser
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

parseB :: Char -> BSParser.Parser [(Text, Text)]
-- ^ Parser for pair
parseB splitter = BSParser.sepBy parsePair (BSParser.char splitter)
  where
    parsePair :: BSParser.Parser (Text, Text)
    parsePair = do
      key <- (BSParser.skipSpace *> BSParser.noneOf1 " =" <* BSParser.skipSpace <* BSParser.char '=')
      value <- (BSParser.skipSpace *> BSParser.noneOf1 (splitter : " ") <* BSParser.skipSpace)
      return (decode key, decode value)
    decode = decodeUtf8 . (urlDecode True)

splitAndDecodeB :: Char -> ByteString -> QueryString
-- ^ TODO: Merge with Text
splitAndDecodeB mark bs = case BSParser.parseOnly (parseB mark) bs of
  Right val -> M.fromList val
  Left _    -> M.empty

stripHtml :: L.Text -> L.Text
stripHtml = L.concatMap char
  where
    char '<' = "&lt;"
    char '>' = "&gt;"
    char '&' = "&amp;"
    char '"' = "&quot;"
    char '\'' = "&#39;"
    char x = L.singleton x

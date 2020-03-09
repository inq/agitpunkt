{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Misc.Json where

import           Control.Applicative ((<|>))
import           Data.Char           (chr, isDigit)
import qualified Data.Map.Strict     as M
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Time.Clock     as TC
import           Database.MongoDB    (ObjectId)
import qualified Misc.Parser         as P

-- * Data types
data Json
  = JSString {-# UNPACK #-}!Text
  | JSInt {-# UNPACK #-}!Int
  | JSObject !(M.Map Text Json)
  | JSArray ![Json]
  | JSBoolean !Bool
  | JSObjectId {-# UNPACK #-}!Text
  | JSNil
  deriving (Eq, Show)

-- * Instances
class ToJson a where
  toJson :: a -> Json

-- ^ JSON transformation
instance ToJson TC.UTCTime where
  toJson = JSString . Text.pack . show

instance ToJson ObjectId where
  toJson = JSObjectId . Text.pack . show

instance ToJson Integer where
  toJson = JSInt . fromIntegral

instance ToJson Text where
  toJson = JSString

instance ToJson [Char] where
  toJson = JSString . Text.pack

instance ToJson t =>
         ToJson (Maybe t) where
  toJson (Just a) = toJson a
  toJson Nothing  = JSNil

-- * Parsers
parse :: Text -> Json
-- ^ Parse the given bytestring
parse str =
  case P.parseOnly parseJson str of
    Right val -> val
    Left _    -> undefined

parseJson :: P.Parser Json
-- ^ The actual parser
parseJson =
  parseObject <|> parseArray <|> parseString <|> parseBoolean <|> parseInt
  where
    parseString =
      JSString . Text.pack <$> (op '"' *> P.manyTill anyChar (P.char '"'))
      where
        escapeMap =
          M.fromList
            [ ('"', '"')
            , ('\\', '\\')
            , ('/', '/')
            , ('n', '\n')
            , ('r', '\r')
            , ('f', '\f')
            , ('t', '\t')
            , ('b', '\b')
            ]
        isHexDigit w =
          isDigit w || (w >= 'A' && w <= 'F') || (w >= 'a' && w <= 'f')
        anyChar = escaped <|> P.anyChar
        escaped = do
          c <- P.char '\\' *> P.anyChar
          case M.lookup c escapeMap of
            Just ch -> return ch
            Nothing ->
              if c == 'u'
                then do
                  code <- P.count 4 (P.satisfy isHexDigit)
                  if null code
                    then fail "broken unicode"
                    else return $ chr $ read $ "0x" ++ code
                else fail "broken escape character"
    spaces = P.skipWhile P.isHorizontalSpace
    parseArray =
      JSArray <$>
      (op '[' *> P.sepBy (spaces *> parseJson <* spaces) (P.char ',') <* cl ']')
    parseObject =
      JSObject . M.fromList <$>
      (op '{' *> P.sepBy pair (P.char ',') <* cl '}')
      where
        pair = do
          key <- rawString <$> (spaces *> parseString <* spaces <* P.char ':')
          value <- spaces *> parseJson <* spaces
          return (key, value)
        rawString (JSString x) = x
        rawString _            = error "rawString: must receive JSString"
    parseBoolean = JSBoolean <$> (parseTrue <|> parseFalse)
      where
        parseTrue = P.try (spaces *> P.string "true" *> spaces) >> return True
        parseFalse =
          P.try (spaces *> P.string "false" *> spaces) >> return False
    parseInt = JSInt . read <$> P.try (P.many1 P.digit)
    op c = P.try (spaces *> P.char c)
    cl c = P.char c <* spaces

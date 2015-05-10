{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE EmptyCase         #-}
module Manicure.Json where

import qualified Data.ByteString.Char8          as BS
import qualified Text.Parsec.ByteString         as PB
import qualified Text.Parsec                    as P
import qualified Data.Map.Strict                as M
import Data.Char (chr)
import Control.Applicative ((*>), (<*), (<$>), (<*>))
import Text.Parsec ((<|>))
import Control.Monad

data Json = JSString  BS.ByteString
          | JSInt     Int
          | JSObject  (M.Map BS.ByteString Json)
          | JSArray   [Json]
          | JSBoolean Bool
          deriving (Eq, Show)

parse :: BS.ByteString -> Json
-- ^ Parse the given bytestring
parse str = case P.parse parse_json "" str of
    Left err -> undefined
    Right val -> val

parse_json :: PB.Parser Json
-- ^ The actual parser
parse_json = parse_object <|> parse_array <|> parse_string <|> parse_boolean <|> parse_int
  where
    parse_string = liftM (JSString . BS.pack) $ op '"' *> P.manyTill any_char (P.char '"')
      where
        escape_map = M.fromList [
            ('"',  '"'),
            ('\\', '\\'),
            ('/',  '/'),
            ('n',  '\n'), 
            ('r',  '\r'),
            ('f',  '\f'),
            ('t',  '\t'),
            ('b',  '\b')
          ]
        any_char = escaped <|> P.anyChar
        escaped = do
            P.char '\\'
            c <- P.anyChar
            case M.lookup c escape_map of
                Just ch -> return ch
                Nothing -> if c == 'u'
                    then do 
                        code <- P.count 4 P.hexDigit
                        if null code
                             then fail "broken unicode"
                             else return $ chr $ read $ "0x" ++ code
                    else fail "broken escape character"
        
    parse_array = liftM JSArray $ op '[' *> P.sepBy (P.spaces *> parse_json <* P.spaces) (P.char ',') <* cl ']'
    parse_object = liftM (JSObject . M.fromList) $ op '{' *> P.sepBy pair (P.char ',') <* cl '}'
      where
        pair = do
            key <- P.spaces *> parse_string <* P.spaces <* P.char ':'
            value <- P.spaces *> parse_json <* P.spaces 
            return (raw_string key, value)
        raw_string (JSString x) = x
    parse_boolean = liftM JSBoolean (parse_true <|> parse_false)
      where
        parse_true = do P.try (P.spaces *> P.string "true" *> P.spaces); return True
        parse_false = do P.try (P.spaces *> P.string "false" *> P.spaces); return False
    parse_int = liftM (JSInt . read) $ P.try (P.many1 P.digit)
    op c = P.try (P.spaces *> P.char c)
    cl c = P.char c <* P.spaces


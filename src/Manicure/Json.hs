{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleContexts  #-}
module Manicure.Json where

import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.UTF8             as UTF8
import qualified Data.Attoparsec.ByteString       as AB
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Map.Strict                  as M
import Data.Char (chr)
import Control.Applicative ((*>), (<*), (<$>), (<*>), (<|>))
import Control.Monad

data Json = JSString  BS.ByteString
    | JSInt     Int
    | JSObject  (M.Map BS.ByteString Json)
    | JSArray   [Json]
    | JSBoolean Bool
    deriving (Eq, Show)

parse :: BS.ByteString -> Json
-- ^ Parse the given bytestring
parse str = case AC.parseOnly parse_json str of
    Right val -> val
    Left err -> undefined

parse_json :: AB.Parser Json
-- ^ The actual parser
parse_json = parse_object <|> parse_array <|> parse_string <|> parse_boolean <|> parse_int
  where
    parse_string = liftM (JSString . UTF8.fromString) $ op '"' *> AC.manyTill any_char (AC.char8 '"')
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
        isHexDigit w = (w >= '0' && w <= '9') ||
                       (w >= 'A' && w <= 'F') ||
                       (w >= 'a' && w <= 'f')
        any_char = escaped <|> AC.anyChar
        escaped = do
            AC.char8 '\\'
            c <- AC.anyChar
            case M.lookup c escape_map of
                Just ch -> return ch
                Nothing -> if c == 'u'
                    then do 
                        code <- AC.count 4 (AC.satisfy isHexDigit)
                        if null code
                             then fail "broken unicode"
                             else return $ chr $ read $ "0x" ++ code
                    else fail "broken escape character"
        
    spaces = AB.skipWhile AC.isHorizontalSpace
    parse_array = liftM JSArray $ op '[' *> AC.sepBy (spaces *> parse_json <* spaces) (AC.char8 ',') <* cl ']'
    parse_object = liftM (JSObject . M.fromList) $ op '{' *> AC.sepBy pair (AC.char8 ',') <* cl '}'
      where
        pair = do
            key <- spaces *> parse_string <* spaces <* AC.char8 ':'
            value <- spaces *> parse_json <* spaces 
            return (raw_string key, value)
        raw_string (JSString x) = x
    parse_boolean = liftM JSBoolean (parse_true <|> parse_false)
      where
        parse_true = do AC.try (spaces *> AC.string "true" *> spaces); return True
        parse_false = do AC.try (spaces *> AC.string "false" *> spaces); return False
    parse_int = liftM (JSInt . read) $ AC.try (AC.many1 AC.digit)
    op c = AC.try (spaces *> AC.char8 c)
    cl c = AC.char c <* spaces

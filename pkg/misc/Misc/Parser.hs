{-# LANGUAGE FlexibleContexts #-}

module Misc.Parser where

import           Control.Applicative       (Alternative)
import           Control.Monad             (MonadPlus)
import qualified Data.Attoparsec.Text      as A
import qualified Data.Attoparsec.Text.Lazy as AL
import           Data.Char                 (ord)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as L
import           Prelude                   hiding (takeWhile)

type Parser = A.Parser

decimal :: Parser Int
decimal = A.decimal

quoted :: Parser Text
quoted = char '"' *> (Text.concat <$> cont) <* char '"'
  where
    cont = do
      c <- peekChar'
      case c of
        '"'  -> return []
        '\\' -> (:) <$> escaped <*> cont
        _    -> (:) <$> noneOf1 "\\\"" <*> cont
    escaped = Text.singleton <$> (char '\\' *> anyChar)

peekChar' :: Parser Char
peekChar' = A.peekChar'

skipSpace :: Parser ()
skipSpace = A.skipSpace

token :: Char -> Parser Char
token c = spaces *> char c <* spaces

isHorizontalSpace :: Char -> Bool
isHorizontalSpace = A.isHorizontalSpace

count
  :: Monad m
  => Int -> m a -> m [a]
count = A.count

digit :: Parser Char
digit = A.digit

satisfy :: (Char -> Bool) -> Parser Char
satisfy = A.satisfy

isToken :: Char -> Bool
isToken w = ord w <= 127 && AL.notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

anyChar :: Parser Char
anyChar = A.anyChar

manyTill
  :: Alternative f
  => f a -> f b -> f [a]
manyTill = A.manyTill

takeTill :: (Char -> Bool) -> Parser Text
takeTill = AL.takeTill

takeTill' :: (Char -> Bool) -> Parser Text
takeTill' = A.takeTill

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile = AL.takeWhile

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 = AL.takeWhile1

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile = AL.skipWhile

endOfLine :: Parser ()
endOfLine = A.endOfLine

isEndOfLine :: Char -> Bool
isEndOfLine = A.isEndOfLine

char :: Char -> Parser Char
char = A.char

many
  :: MonadPlus m
  => m a -> m [a]
many = AL.many'

many1
  :: Alternative f
  => f a -> f [a]
many1 = AL.many1

spaces :: Parser ()
spaces = AL.skipWhile A.isHorizontalSpace

noneOf :: String -> Parser Text
noneOf = takeWhile . AL.notInClass

noneOf1 :: String -> Parser Text
noneOf1 = takeWhile1 . AL.notInClass

try :: Parser a -> Parser a
try = A.try

sepBy
  :: Alternative f
  => f a -> f s -> f [a]
sepBy = A.sepBy

sepBy1
  :: Alternative f
  => f a -> f s -> f [a]
sepBy1 = A.sepBy1

sepBy'
  :: MonadPlus m
  => m a -> m s -> m [a]
sepBy' = A.sepBy'

string :: Text -> Parser Text
string = A.string

parse :: Parser a -> L.Text -> AL.Result a
parse = AL.parse

parseOnly :: Parser a -> Text -> Either String a
parseOnly = A.parseOnly

takeText :: Parser Text
takeText = A.takeText

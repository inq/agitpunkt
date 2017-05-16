{-# LANGUAGE FlexibleContexts #-}

module Misc.Parser.ByteString
  ( module Data.Attoparsec.ByteString.Char8
  , quoted
  , noneOf1
  , token
  , isToken
  ) where

import qualified Data.Attoparsec.ByteString.Char8      as BSParser
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8          as BS
import           Data.Attoparsec.ByteString.Char8      (Parser)
import           GHC.Word (Word8)
import Data.Attoparsec.ByteString.Char8
import Data.Char (chr)

quoted :: Parser ByteString
quoted = BSParser.char '"' *> (BS.concat <$> cont) <* BSParser.char '"'
  where
    cont = do
      c <- BSParser.peekChar'
      case c of
        '"'  -> return []
        '\\' -> (:) <$> escaped <*> cont
        _    -> (:) <$> noneOf1 "\\\"" <*> cont
    escaped = BS.singleton <$> (BSParser.char '\\' *> BSParser.anyChar)

token :: Char -> Parser Char
token c = BSParser.skipSpace *> BSParser.char c <* BSParser.skipSpace

noneOf1 :: String -> Parser ByteString
noneOf1 = BSParser.takeWhile1 . BSParser.notInClass

isToken :: Word8 -> Bool
isToken w =
  w <= 127 && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" (chr $ iw)
  where
  iw = fromIntegral w :: Int

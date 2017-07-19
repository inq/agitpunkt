{-# LANGUAGE FlexibleContexts #-}

module Misc.Parser.LazyByteString
  ( module Data.Attoparsec.ByteString.Lazy
  , token
  , isToken
  ) where

import qualified Data.Attoparsec.ByteString.Char8 as BSParser
import           Data.Attoparsec.ByteString.Lazy
import           GHC.Word                         (Word8)

token :: Char -> Parser Char
token c = BSParser.skipSpace *> BSParser.char c <* BSParser.skipSpace

isToken :: Word8 -> Bool
isToken w =
  w <= 127 && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

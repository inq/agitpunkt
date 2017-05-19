{-# LANGUAGE FlexibleContexts #-}

module Misc.Parser.LazyByteString
  ( module Data.Attoparsec.ByteString.Lazy
  , token
  , isToken
  ) where

import           GHC.Word (Word8)
import Data.Attoparsec.ByteString.Lazy 
import qualified Data.Attoparsec.ByteString.Char8 as BSParser

token :: Char -> Parser Char
token c = BSParser.skipSpace *> BSParser.char c <* BSParser.skipSpace

isToken :: Word8 -> Bool
isToken w =
  w <= 127 && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

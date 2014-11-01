module Manicure.Request (
  Request,
  parse
) where

import qualified Data.ByteString.Char8 as BS

data Request = Request {
  method :: Method,
  version :: Version,
  headers :: RequestHeaders,
  tmp :: [BS.ByteString]
} deriving (Show)

data Method = GET
  | POST
  | PUT
  | DELETE
  | PATCH
  | TRACE
  | OPTIONS
  | HEAD
  | CONNECT
  deriving (Show)

data Version = Version {
  major :: !Int,
  minor :: !Int
} deriving (Show)

type RequestHeaders = [Header]
type Header = (BS.ByteString, BS.ByteString)

parse :: BS.ByteString -> Request
parse ipt = 
    case splitLines ipt of
        head : tail -> 
            Request method (Version 1 1) ([(BS.pack "Hello", BS.pack "World")]) tail
          where
            method = parseHead head

parseHead :: BS.ByteString -> Method
parseHead str =
    case (BS.index str 0, BS.index str 1) of
        ('G', _) -> GET
        ('P', 'O') -> POST
        ('H', _) -> HEAD
        ('P', 'U') -> PUT
        ('D', 'E') -> DELETE
        ('T', _) -> TRACE
        ('C', _) -> CONNECT
        ('O', _) -> OPTIONS
        ('P', _) -> PATCH

parseTail :: [BS.ByteString] -> [BS.ByteString]
parseTail tail = 
    []

splitLines :: BS.ByteString -> [BS.ByteString]
splitLines str =
    case BS.elemIndex '\r' str of
        Just i | i > 2 -> BS.take i str : (splitLines $ BS.drop (i + 2) str)
        Just i         -> [BS.drop 4 str]
        Nothing        -> []

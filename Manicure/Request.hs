module Manicure.Request (
  Request,
  parse
) where

import qualified Data.ByteString.Char8 as BS
import Data.Char

data Request = Request {
  method  :: Method,
  version :: Version,
  uri     :: BS.ByteString,
  headers :: RequestHeaders
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

offset :: Method -> Int
offset GET     = 4
offset POST    = 5
offset PUT     = 4
offset DELETE  = 6
offset PATCH   = 6
offset TRACE   = 6
offset OPTIONS = 7
offset HEAD    = 5
offset CONNECT = 7

parse :: BS.ByteString -> Request
parse ipt = 
    parseHead head $ parseTail tail
  where 
    head : tail = splitLines ipt
        
parseHead :: BS.ByteString -> RequestHeaders -> Request
parseHead str =
    Request method version uri
  where
    method = case BS.index str 0 of
        'G' -> GET
        'D' -> DELETE
        'C' -> CONNECT
        _   -> case BS.index str 1 of
            'O' -> POST
            'U' -> PUT
            'A' -> PATCH
            'P' -> OPTIONS
            'E' -> HEAD
            _   -> TRACE
    length = BS.length str
    uri = BS.drop (offset method) $ BS.take (length - 9) str
    version = Version (digitToInt $ BS.index str (length - 3)) (digitToInt $ BS.index str (length - 1))

parseTail :: [BS.ByteString] -> RequestHeaders
parseTail list = 
    map split list 
  where
    split line = 
        (head, tail)
      where 
        idx = case BS.elemIndex ':' line of
            Just i -> i
            Nothing -> 0
        head = BS.take idx line
        tail = BS.drop (idx + 1) line

splitLines :: BS.ByteString -> [BS.ByteString]
splitLines str =
    case BS.elemIndex '\r' str of
        Just i | i > 2 -> BS.take i str : (splitLines $ BS.drop (i + 2) str)
        Just i         -> [BS.drop 4 str]
        Nothing        -> []

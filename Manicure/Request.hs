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

data Method
  = GET
  | POST
  | PUT
  | DELETE
  deriving (Show)

data Version = Version {
  major :: !Int,
  minor :: !Int
} deriving (Show)

type RequestHeaders = [Header]
type Header = (BS.ByteString, BS.ByteString)

parse :: BS.ByteString -> Request
parse ipt = 
    Request GET (Version 1 1) ([(BS.pack "Hello", BS.pack "World")]) $ splitLines ipt

splitLines :: BS.ByteString -> [BS.ByteString]
splitLines str = 
    split [] str
  where
    split :: [BS.ByteString] -> BS.ByteString -> [BS.ByteString]
    split list str = do
        case BS.elemIndex '\r' str of
            Just i -> split ((BS.take (i - 1) str) : list) $ BS.drop (i + 2) str
            Nothing -> str : list

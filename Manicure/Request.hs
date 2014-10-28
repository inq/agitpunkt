module Manicure.Request (
  Request,
  parse
) where

import qualified Data.ByteString.Char8 as BS

data Request = Request {
  method :: Method,
  version :: Version,
  headers :: RequestHeaders
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
    Request GET (Version 1 1) ([(BS.pack "Hello", BS.pack "World")])

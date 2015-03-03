{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
module Manicure.Request (
  Request,
  Method,
  strToMethod,
  parse,
  method,
  uri,
  post,
  extract_cookie
) where

import qualified Language.Haskell.TH.Syntax     as TS
import qualified Data.ByteString.Char8          as BS
import qualified Manicure.Http                  as Http
import qualified Network.Socket                 as NS
import qualified Data.Char                      as C
import qualified Data.Map                       as M
import qualified Network.HTTP.Types.URI         as URI
import qualified Data.Either                    as E

type PostData = M.Map BS.ByteString BS.ByteString

data Request = Request {
  method  :: Method,
  version :: Http.Version,
  uri     :: BS.ByteString,
  headers :: RequestHeaders,
  post    :: PostData,
  request_socket :: NS.Socket
} deriving (Show)

data Method = GET | POST | PUT | DELETE | PATCH
  | TRACE | OPTIONS | HEAD | CONNECT
  deriving (Show, Eq, Ord)

instance TS.Lift Method where
    lift GET     = [| GET |]
    lift POST    = [| POST |]
    lift PUT     = [| PUT |]
    lift DELETE  = [| DELETE |]
    lift PATCH   = [| PATCH |]
    lift TRACE   = [| TRACE |]
    lift OPTIONS = [| OPTIONS |]
    lift CONNECT = [| CONNECT |]

strToMethod :: String -> Method
-- ^ Convert strings to the corresponding method
strToMethod "GET" = GET
strToMethod "POST" = POST

type RequestHeaders = [Header]
type Header = (BS.ByteString, BS.ByteString)

extract_cookie :: Request -> BS.ByteString
-- ^ Extract cookie from the request header
extract_cookie req = 
    find_cookie $ headers req
  where
    find_cookie (("Cookie", context) : _) = context
    find_cookie (head : tail)             = find_cookie tail
    find_cookie []                        = ""

parse :: BS.ByteString -> NS.Socket -> Request
-- ^ Read and parse the data from socket to make the Request data
parse ipt socket = 
    parseHead head (parseTail tail) post socket
  where 
    lines = splitLines ipt
    post  = build $ last lines
    head : tail = init lines
    build bs = M.fromList $ map transform (BS.split '&' bs)
      where
        transform line = pair
          where
            idx = case BS.elemIndex '=' line of
                Just i -> i
                Nothing -> 0
            pair = (decode $ BS.take idx line, decode $ BS.drop (idx + 1) line)
              where
                decode = URI.urlDecode True
        
parseHead :: BS.ByteString -> RequestHeaders -> PostData -> NS.Socket -> Request
-- ^ Parse the first line of the HTTP header
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
      where
        offset :: Method -> Int
        offset GET     = 4
        offset POST    = 5
        offset PUT     = 4
        offset HEAD    = 5
        offset OPTIONS = 7
        offset CONNECT = 7
        offset _       = 6
    version = Http.Version 
        (C.digitToInt $ BS.index str (length - 3)) 
        (C.digitToInt $ BS.index str (length - 1))

parseTail :: [BS.ByteString] -> RequestHeaders
-- ^ Parse the rest part of the HTTP header
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
-- ^ Split the lines from the HTTP header
splitLines str =
    case BS.elemIndex '\r' str of
        Just i | i > 2 -> BS.take i str : (splitLines $ BS.drop (i + 2) str)
        Just i         -> [BS.drop 2 str]
        Nothing        -> [""]

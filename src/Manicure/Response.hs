{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Manicure.Response where

import qualified Language.Haskell.TH.Syntax     as TS
import qualified Data.ByteString.Char8          as BS
import qualified Manicure.Http                  as Http
import qualified Manicure.Request               as Req
import qualified Manicure.Database              as DB

type Handler = [BS.ByteString] -> Action
type Action = DB.Connection -> Req.Request -> IO Response

data Response = Response {
  version :: Http.Version,
  status_code :: Int,
  cookies :: [BS.ByteString],
  content :: BS.ByteString
} deriving Show

instance Show (BS.ByteString -> Handler) where
    show _ = ""
instance Show Handler where
    show _ = ""

render :: Response -> BS.ByteString
-- ^ Render to the ByteString
render (Response version status_code cookies content) =
    BS.concat (
      ["HTTP/1.0 200 OK\r\n"] ++ 
      map cookie_to_string cookies ++
      [
        "Content-Length: ", BS.pack $ show $ BS.length content,
        "\r\n\r\n", 
        content
      ])
  where
    cookie_to_string cookie = BS.concat ["Set-Cookie: ", cookie, "; path=/\r\n"]

defaultVersion :: Http.Version
-- ^ The default version is HTTP 1.1
defaultVersion = Http.Version 1 1

success :: BS.ByteString -> [BS.ByteString] -> Response
-- ^ Generate a Response data which represents 200 OK
success bs cookies = Response defaultVersion 200 cookies bs

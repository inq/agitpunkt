{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Manicure.Response where

import qualified Language.Haskell.TH.Syntax     as TS
import qualified Data.ByteString.Char8          as BS
import qualified Manicure.Http                  as Http
import qualified Manicure.Request               as Req
import qualified Manicure.Database              as DB

type Handler = DB.Connection -> Req.Request -> IO Response

data Response = Response {
  version :: Http.Version,
  status_code :: Int,
  content :: BS.ByteString
} deriving Show

data Renderable =
    R  Handler |
    BR (BS.ByteString -> Handler)
  deriving Show

instance Show (BS.ByteString -> Handler) where
    show _ = ""
instance Show Handler where
    show _ = ""

process :: Renderable -> Handler
process (R a) = a
process (BR a) = a ""

render :: Response -> BS.ByteString
render (Response version status_code content) =
  BS.concat [
    "HTTP/1.0 200 OK\r\nContent-Length: ", 
    BS.pack $ show $ BS.length content,
    "\r\n\r\n", 
    content
  ]

defaultVersion :: Http.Version
defaultVersion = Http.Version 1 1

success :: BS.ByteString -> Response
success bs = Response defaultVersion 200 bs

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Response where

import qualified Core.Http            as Http
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Text            (Text)
import           Data.Text.Encoding   (encodeUtf8)

-- * Data types
data Response = Response
  { version    :: {-# UNPACK #-}!Http.Version
  , statusCode :: {-# UNPACK #-}!Int
  , cookies    :: [Text]
  , content    :: {-# UNPACK #-}!Text
  } deriving (Show)

cookieToString :: [Text] -> [ByteString]
-- ^ Convert cookies to string
cookieToString =
  map (\x -> BS.concat ["Set-Cookie: ", encodeUtf8 x, "; path=/\r\n"])

renderContent :: Response -> [ByteString]
-- ^ Render the content
renderContent (Response _ _ cookies' content') =
  cookieToString cookies' ++
  ["Content-Length: ", UTF8.fromString $ show length', "\r\n\r\n", utf8Content]
  where
    utf8Content = encodeUtf8 content'
    length' = BS.length utf8Content

render :: Response -> ByteString
-- ^ Render to the ByteString
render r@(Response _ 200 _ _) =
  BS.concat ("HTTP/1.0 200 OK\r\n" : renderContent r)
render r@(Response _ 404 _ _) =
  BS.concat ("HTTP/1.0 404 Not Found\r\n" : renderContent r)
render (Response _ 303 _cookies url) =
  BS.concat $
  ["HTTP/1.0 303 See Other\r\n"] ++
  cookieToString _cookies ++ ["Location: ", encodeUtf8 url, "\r\n\r\n"]
render r@Response {} =
  BS.concat ("HTTP/1.0 500 Internal Error\r\n" : renderContent r)

defaultVersion :: Http.Version
-- ^ The default version is HTTP 1.1
defaultVersion = Http.Version 1 1

success :: Text -> [Text] -> Response
-- ^ Generate a Response data which represents 200 OK
success content' cookies' = Response defaultVersion 200 cookies' content'

error :: Int -> Text -> Response
-- ^ Error page
error code = Response defaultVersion code []

redirect :: Text -> [Text] -> Response
-- ^ Redirect to the specific URL
redirect url _cookies = Response defaultVersion 303 _cookies url

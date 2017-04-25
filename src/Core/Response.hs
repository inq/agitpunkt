{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Response where

import qualified Core.Http as Http
import           Data.Text (Text)
import qualified Data.Text as Text

-- * Data types
data Response = Response
  { version    :: {-# UNPACK #-}!Http.Version
  , statusCode :: {-# UNPACK #-}!Int
  , cookies    :: [Text]
  , content    :: {-# UNPACK #-}!Text
  } deriving (Show)

cookieToString :: [Text] -> [Text]
-- ^ Convert cookies to string
cookieToString = map (\x -> Text.concat ["Set-Cookie: ", x, "; path=/\r\n"])

renderContent :: Response -> [Text]
-- ^ Render the content
renderContent (Response _ _ _cookies _content) =
  cookieToString _cookies ++
  [ "Content-Length: "
  , Text.pack $ show $ Text.length _content
  , "\r\n\r\n"
  , _content
  ]

render :: Response -> Text
-- ^ Render to the ByteString
render r@(Response _ 200 _ _) =
  Text.concat ("HTTP/1.0 200 OK\r\n" : renderContent r)
render r@(Response _ 404 _ _) =
  Text.concat ("HTTP/1.0 404 Not Found\r\n" : renderContent r)
render (Response _ 303 _cookies url) =
  Text.concat $
  ["HTTP/1.0 303 See Other\r\n"] ++
  cookieToString _cookies ++ ["Location: ", url, "\r\n\r\n"]
render r@Response {} =
  Text.concat ("HTTP/1.0 500 Internal Error\r\n" : renderContent r)

defaultVersion :: Http.Version
-- ^ The default version is HTTP 1.1
defaultVersion = Http.Version 1 1

success :: Text -> [Text] -> Response
-- ^ Generate a Response data which represents 200 OK
success bs _cookies = Response defaultVersion 200 _cookies bs

error :: Int -> Text -> Response
-- ^ Error page
error code = Response defaultVersion code []

redirect :: Text -> [Text] -> Response
-- ^ Redirect to the specific URL
redirect url _cookies = Response defaultVersion 303 _cookies url

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Core.Request where

import           Control.Applicative        (many)
import           Control.Monad              (when)
import qualified Core.Http                  as Http
import           Core.Request.Content       (Content, mkContent)
import qualified Data.Attoparsec.Text.Lazy  as AL
import qualified Data.Char                  as C
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Encoding         (decodeUtf8)
import qualified Data.Text.Lazy             as L
import qualified Data.Text.Read             as TextRead
import           Language.Haskell.TH.Syntax (Lift, lift)
import qualified Misc.Parser                as P
import           Misc.TextUtil              (QueryString, splitAndDecode)
import           Network.Socket             (Socket)
import           Network.Socket.ByteString  (recv)

-- * Data types
data Request = Request
  { method        :: Method
  , version       :: Http.Version
  , uri           :: Text
  , headers       :: RequestHeaders
  , content       :: Content
  , queryStr      :: QueryString
  , requestSocket :: Socket
  } deriving (Show)

data Method
  = GET
  | POST
  | PUT
  | DELETE
  | PATCH
  | TRACE
  | OPTIONS
  | HEAD
  | CONNECT
  deriving (Show, Read, Eq, Ord)

instance Lift Method where
  lift GET     = [|GET|]
  lift POST    = [|POST|]
  lift PUT     = [|PUT|]
  lift DELETE  = [|DELETE|]
  lift PATCH   = [|PATCH|]
  lift TRACE   = [|TRACE|]
  lift OPTIONS = [|OPTIONS|]
  lift HEAD    = [|HEAD|]
  lift CONNECT = [|CONNECT|]

type RequestHeaders = [Header]

type Header = (Text, Text)

type Lines = ([Text], Text)

receiveHeader :: Socket -> IO Lines
-- ^ Receive header from the socket
--   TODO: Bytestring must be used.
receiveHeader fd = do
  buf <- decodeUtf8 <$> recv fd 4096
  when (Text.length buf == 0) $ error "Disconnected"
  receiveHeader' [] buf
  where
    receiveHeader' res buffer = do
      let (line, remaining) = Text.breakOn "\r\n" buffer
      let remaining' = Text.drop 2 remaining
      if Text.length line == 0
        then return (res, remaining')
        else if Text.length remaining' == 0
               then do
                 buf <- decodeUtf8 <$> recv fd 4096
                 if Text.length buf == 0
                   then error "Disconnected"
                   else receiveHeader' res $ Text.append remaining' buf
               else receiveHeader' (line : res) remaining'

extractCookie :: Request -> M.Map Text Text
-- ^ Extract cookie from the request header
extractCookie req = findCookie $ headers req
  where
    findCookie (("Cookie", context):_) = splitAndDecode ';' context
    findCookie (_:t)                   = findCookie t
    findCookie []                      = M.empty

parse :: L.Text -> Socket -> Request
-- ^ Read and parse the data from socket to make the Request data
parse ipt = parseHead _head res content'
  where
    content' =
      mkContent (M.lookup "Content-Type" $ M.fromList res) $
      L.take contentLength pdata
    contentLength =
      case M.lookup "Content-Length" $ M.fromList res of
        Just len ->
          case (TextRead.decimal len :: Either String (Int, Text)) of
            Right num -> fromIntegral $ fst num
            Left _    -> error "parse error"
        Nothing -> 0
    (_head, res, pdata) =
      case P.parse request ipt of
        AL.Done remaining (h, r) -> (h, r, remaining)
        _                        -> error "parse error"
    request =
      (,) <$> (P.takeTill P.isEndOfLine <* P.endOfLine) <*>
      (many header <* P.endOfLine)
    header =
      (,) <$>
      (P.takeWhile P.isToken <* P.char ':' <* P.skipWhile P.isHorizontalSpace) <*>
      (P.takeTill P.isEndOfLine <* P.endOfLine)

splitLines :: Text -> [Text]
-- ^ Split the lines from the HTTP header
splitLines str =
  case Text.findIndex (== '\r') str of
    Just i
      | i > 2 -> Text.take i str : splitLines (Text.drop (i + 2) str)
    Just _ -> [Text.drop 2 str]
    Nothing -> [""]

parseHead :: Text -> RequestHeaders -> Content -> Socket -> Request
-- ^ Parse the first line of the HTTP header
parseHead str _headers content' =
  Request _method _version _uri _headers content' queryString
  where
    _method =
      case Text.index str 0 of
        'G' -> GET
        'D' -> DELETE
        'C' -> CONNECT
        _ ->
          case Text.index str 1 of
            'O' -> POST
            'U' -> PUT
            'A' -> PATCH
            'P' -> OPTIONS
            'E' -> HEAD
            _   -> TRACE
    _length = Text.length str
    uriLong = Text.drop (offset _method) $ Text.take (_length - 9) str
      where
        offset :: Method -> Int
        offset GET     = 4
        offset POST    = 5
        offset PUT     = 4
        offset HEAD    = 5
        offset OPTIONS = 7
        offset CONNECT = 7
        offset _       = 6
    (_uri, queryStringRaw) = Text.break (== '?') uriLong
    queryStringTail
      | Text.null queryStringRaw = ""
      | Text.head queryStringRaw == '?' = Text.tail queryStringRaw
      | otherwise = ""
    queryString = splitAndDecode '&' queryStringTail
    _version =
      Http.Version
        (C.digitToInt $ Text.index str (_length - 3))
        (C.digitToInt $ Text.index str (_length - 1))

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Core.Request where

import           Control.Applicative        (many)
import qualified Core.Http                  as Http
import           Core.Request.Content       (Content, mkContent)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as LazyBS
import qualified Data.Char                  as C
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Encoding         (decodeUtf8)
import qualified Data.Text.Read             as TextRead
import           Language.Haskell.TH.Syntax (Lift, lift)
import qualified Misc.Parser.ByteString     as BSParser
import qualified Misc.Parser.LazyByteString as LazyBSParser
import           Misc.TextUtil              (QueryString, splitAndDecode,
                                             splitAndDecodeB)
import           Network.Socket             (Socket)

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

type Lines = ([BS.ByteString], BS.ByteString)

extractCookie :: Request -> M.Map Text Text
-- ^ Extract cookie from the request header
extractCookie req = findCookie $ headers req
  where
    findCookie (("Cookie", context):_) = splitAndDecode ';' context
    findCookie (_:t)                   = findCookie t
    findCookie []                      = M.empty

parse :: LazyBS.ByteString -> Socket -> Request
-- ^ Read and parse the data from socket to make the Request data
parse ipt = parseHead head' res content'
  where
    content' =
      mkContent (M.lookup "Content-Type" $ M.fromList res) $
        LazyBS.take contentLength pdata
    contentLength =
      case M.lookup "Content-Length" $ M.fromList res of
        Just len ->
          case (TextRead.decimal len :: Either String (Int, Text)) of
            Right num -> fromIntegral $ fst num
            Left _    -> error "parse error"
        Nothing -> 0
    (head', res, pdata) =
      case LazyBSParser.parse request ipt of
        LazyBSParser.Done remaining (h, r) -> (h, r, remaining)
        _                                  -> error "parse error"
    request = (,)
      <$> (LazyBSParser.takeTill BSParser.isEndOfLine
           <* BSParser.endOfLine)
      <*> (many header
           <* BSParser.endOfLine)
    header = (,)
      <$> (decodeUtf8 <$> LazyBSParser.takeWhile BSParser.isToken
           <* BSParser.char ':'
           <* LazyBSParser.skipWhile BSParser.isHorizontalSpace)
      <*> (decodeUtf8 <$> LazyBSParser.takeTill BSParser.isEndOfLine
           <* BSParser.endOfLine)

splitLines :: Text -> [Text]
-- ^ Split the lines from the HTTP header
splitLines str =
  case Text.findIndex (== '\r') str of
    Just i
      | i > 2 -> Text.take i str : splitLines (Text.drop (i + 2) str)
    Just _ -> [Text.drop 2 str]
    Nothing -> [""]

parseHead :: ByteString -> RequestHeaders -> Content -> Socket -> Request
-- ^ Parse the first line of the HTTP header
parseHead str headers' content' =
  Request method' version' (decodeUtf8 uri') headers' content' queryString
  where
    method' =
      case BS.index str 0 of
        'G' -> GET
        'D' -> DELETE
        'C' -> CONNECT
        _ ->
          case BS.index str 1 of
            'O' -> POST
            'U' -> PUT
            'A' -> PATCH
            'P' -> OPTIONS
            'E' -> HEAD
            _   -> TRACE
    length' = BS.length str
    uriLong = BS.drop (offset method') $ BS.take (length' - 9) str
      where
        offset :: Method -> Int
        offset GET     = 4
        offset POST    = 5
        offset PUT     = 4
        offset HEAD    = 5
        offset OPTIONS = 7
        offset CONNECT = 7
        offset _       = 6
    (uri', queryStringRaw) = BS.break (== '?') uriLong
    queryStringTail
      | BS.null queryStringRaw = ""
      | BS.head queryStringRaw == '?' = BS.tail queryStringRaw
      | otherwise = ""
    queryString = splitAndDecodeB '&' queryStringTail
    version' =
      Http.Version
        (C.digitToInt $ BS.index str (length' - 3))
        (C.digitToInt $ BS.index str (length' - 1))

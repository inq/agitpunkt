{-# LANGUAGE OverloadedStrings #-}

module Core.Request.Content where

import           Control.Applicative              (many)
import qualified Core.Request.ContentDisposition  as Cont
import qualified Data.Attoparsec.ByteString.Lazy  as LazyBSParser
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.Char8            as BS
import qualified Data.Map                         as M
import qualified Data.Text                        as Text
import           Data.Text                        (Text)
import qualified Data.ByteString.Lazy             as LazyBS
import qualified Misc.Parser                      as P
import qualified Misc.Parser.ByteString           as BSParser
import           Misc.TextUtil                    (splitAndDecodeB)
import           Data.Text.Encoding               (encodeUtf8, decodeUtf8)

-- * Data types
type Content = M.Map Text Context

data Context
  = MkText Text
  | MkFile { filename    :: Maybe Text
          ,  contentType :: Maybe Text
          ,  content     :: ByteString}
  deriving (Eq, Show)

getBoundary :: P.Parser ByteString
-- ^ Parse and read the boundary
getBoundary =
  P.string "multipart/form-data" *>
  P.skipSpace *>
  P.char ';' *>
  P.skipSpace *>
  P.string "boundary=" *>
  (encodeUtf8 <$> P.noneOf1 " ")

-- * Private functions for parseMultipart
collectHeaders :: ByteString -> BSParser.Parser (M.Map Text Text)
collectHeaders boundary = do
  _bdr <- consumeBoundary
  (M.fromList <$> many header) <* BSParser.endOfLine
  where
    header =
      (,) <$>
       (Text.toLower . decodeUtf8 <$> LazyBSParser.takeWhile BSParser.isToken <* BSParser.char ':' <* BSParser.skipSpace) <*>
       (decodeUtf8 <$> LazyBSParser.takeTill BSParser.isEndOfLine <* BSParser.endOfLine)
    consumeBoundary = LazyBSParser.string $ BS.concat ["--", boundary, "\r\n"]

nameAndFilename :: M.Map Text Text -> BSParser.Parser (Maybe Text, Maybe Text)
nameAndFilename headers =
  case M.lookup "content-disposition" headers of
    Just disp ->
      case P.parseOnly Cont.parse disp of
        Right val -> return (Cont.name val, Cont.filename val)
        Left _    -> return (Nothing, Nothing)
    _ -> return (Nothing, Nothing)


parseMultipart :: ByteString -> BSParser.Parser Content
parseMultipart boundary = do
  headers <- collectHeaders boundary
  (name, fileName) <- nameAndFilename headers
  cont <-
    BS.pack <$> LazyBSParser.manyTill BSParser.anyChar
      (LazyBSParser.try $ LazyBSParser.string "\r\n--") <* LazyBSParser.string boundary
  return $
    M.fromList $
    case name of
      Just n ->
        [ ( n
          , MkFile
            { filename = fileName
            , contentType = M.lookup "content-type" headers
            , content = cont
            })
        ]
      Nothing -> []

mkContent :: Maybe Text -> LazyBS.ByteString -> Content
-- ^ TODO: Make simple
mkContent contType cont = case contType of
  Just "application/x-www-form-urlencoded" ->
    M.map MkText $ splitAndDecodeB '&' $ LazyBS.toStrict cont
  Just x ->
    case P.parseOnly getBoundary x of
      Left _ -> M.empty
      Right b -> case LazyBSParser.parse (parseMultipart b) cont of
        LazyBSParser.Done _ res -> res
        LazyBSParser.Fail {}    -> M.empty
  _ -> error "FATAL: Cannot read content type"

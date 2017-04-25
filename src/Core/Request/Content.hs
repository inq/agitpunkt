{-# LANGUAGE OverloadedStrings #-}

module Core.Request.Content where

import           Control.Applicative             (many)
import qualified Core.Request.ContentDisposition as Cont
import qualified Data.Attoparsec.Text.Lazy       as AL
import           Data.Char                       (toLower)
import qualified Data.Map                        as M
import           Data.Maybe                      (fromJust)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Lazy                  as L
import qualified Misc.Parser                     as P
import           Misc.TextUtil                   (QueryString, splitAndDecode)

-- * Data types
type Content = M.Map Text Context

data Context
  = MkText Text
  | MkFile { filename    :: Maybe Text
          ,  contentType :: Maybe Text
          ,  content     :: Text}
  deriving (Eq, Show)

getBoundary :: P.Parser Text
-- ^ Parse and read the boundary
getBoundary =
  P.string "multipart/form-data" *> P.skipSpace *> P.char ';' *> P.skipSpace *>
  P.string "boundary=" *>
  P.noneOf1 " "

parseMultipart :: Text -> P.Parser Content
parseMultipart boundary = do
  _ <- P.string "--" <* P.string boundary <* P.string "\r\n"
  headers <- (M.fromList <$> many header) <* P.endOfLine
  (name, fname) <-
    case M.lookup "content-disposition" headers of
      Just disp ->
        case P.parseOnly Cont.parse disp of
          Right val -> return (Cont.name val, Cont.filename val)
          Left _    -> return (Nothing, Nothing)
      _ -> return (Nothing, Nothing)
  cont <-
    Text.pack <$> P.manyTill P.anyChar (P.try $ P.string "\r\n--") <*
    P.string boundary
  return $
    M.fromList $
    case name of
      Just n ->
        [ ( n
          , MkFile
            { filename = fname
            , contentType = M.lookup "content-type" headers
            , content = cont
            })
        ]
      Nothing -> []
  where
    header =
      (,) <$>
      (Text.map toLower <$>
       (P.takeWhile P.isToken <* P.char ':' <* P.skipWhile P.isHorizontalSpace)) <*>
      (P.takeTill P.isEndOfLine <* P.endOfLine)

fromQS :: QueryString -> Content
fromQS = M.map MkText

mkContent :: Maybe Text -> L.Text -> Content
mkContent contType cont =
  case fromJust contType of
    "application/x-www-form-urlencoded" ->
      fromQS $ splitAndDecode '&' $ L.toStrict cont
    x ->
      case P.parseOnly getBoundary x of
        Left _ -> M.empty
        Right b ->
          case P.parse (parseMultipart b) cont of
            AL.Done _ res -> res
            AL.Fail {}    -> M.empty

lookup :: Text -> Content -> Maybe Context
lookup = M.lookup

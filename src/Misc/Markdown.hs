{-# LANGUAGE OverloadedStrings #-}

module Misc.Markdown where

import           Control.Applicative       ((<|>))
import           Data.Attoparsec.Text.Lazy (Result (Done))
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as L
import qualified Misc.Parser               as P
import           Misc.TextUtil             (stripHtml)

-- * Data types
newtype Markdown =
  Markdown [Item]
  deriving (Eq, Show)

data Img
  = Img !L.Text
        !L.Text
  deriving (Eq, Show)

data Item
  = H5 !L.Text
  | H4 !L.Text
  | H3 !L.Text
  | H2 !L.Text
  | H1 !L.Text
  | Container [Img]
  | Quote !L.Text
  | Paragraph !L.Text
  | Snippet !L.Text
            ![L.Text]
  deriving (Eq, Show)

-- * Parsers
parse :: L.Text -> Maybe Markdown
-- ^ Parse the given bytestring
parse str =
  case P.parse parseMarkdown str of
    Done _ val -> Just val
    _          -> Nothing

parseItem :: P.Parser Item
-- ^ The subparser
parseItem =
  parseHeader <|> parseContainer <|> parseSnippet <|> parseQuote <|> parseImage <|> parseParagraph
  where
    parseContainer = do
    -- ^ Contain the img tags.
      open <-
        L.fromStrict <$>
        P.try (P.string "{{{" <* P.string "\r\n")
      inner <- parseLine
      return $ Container inner
      where
      parseLine =
        parseEnd <|>
        ((:) <$> parseImg <*> parseLine)
      parseEnd = do
        _ <- P.try (P.string "}}}")
        return []
    parseSnippet = do
      open <-
        L.fromStrict <$>
        P.try (P.string "```" *> P.noneOf1 "\r\n" <* P.string "\r\n")
      res <- (map stripHtml) <$> parseLine
      return $ Snippet open res
      where
      parseLine =
        parseEnd <|>
        (((:) . L.fromStrict) <$> (P.noneOf "\r\n" <* P.string "\r\n") <*> parseLine)
      parseEnd = do
        _ <- P.try (P.string "```")
        return []
    parseImg = do
    -- ^ Img tag. It must be wrapped by Container.
      alt <-
        L.fromStrict <$>
        P.try
          (P.char '!' *> P.spaces *> P.noneOf1 ";" <* P.char ';' <* P.spaces)
      uri <- L.fromStrict <$> P.noneOf1 "\r" <* P.string "\r\n"
      return $ Img alt uri
    parseImage = do
      img <- parseImg
      return $ Container [ img ]
    parseHeader = do
      sharps <- P.try (P.many1 (P.char '#')) <* P.spaces
      rest <- L.fromStrict <$> P.noneOf1 "\r\n"
      return $
        case length sharps of
          1 -> H1 rest
          2 -> H2 rest
          3 -> H3 rest
          4 -> H4 rest
          5 -> H5 rest
          _ -> error "not implemented"
    parseQuote = do
      _ <- P.try (P.char '>') <* P.spaces
      rest <- L.fromStrict <$> P.noneOf1 "\r\n"
      return $ Quote rest
    parseParagraph = do
      rest <- L.fromStrict <$> P.noneOf1 "\r\n"
      return $ Paragraph rest

parseMarkdown :: P.Parser Markdown
-- ^ The actual parser
parseMarkdown = do
  items <- P.sepBy parseItem (P.many1 (P.string "\r\n"))

  return $ Markdown items

toHtml :: Markdown -> L.Text
-- ^ Generate html
toHtml (Markdown items) = L.concat $ map toStr items

toStr :: Item -> L.Text
-- ^ Convert item to string
toStr (H5 str) = L.concat ["<h5>", str, "</h5>"]
toStr (H4 str) = L.concat ["<h4>", str, "</h4>"]
toStr (H3 str) = L.concat ["<h3>", str, "</h3>"]
toStr (H2 str) = L.concat ["<h2>", str, "</h2>"]
toStr (H1 str) = L.concat ["<h1>", str, "</h1>"]
toStr (Container imgs) =
  L.concat (["<div class='paragraph", (L.pack $ show $ length imgs), "'>"]
  ++ renderImgs imgs ++ ["</div>"])
  where
  renderImgs (Img alt uri : t) =
    L.concat ["<img class='content-img' alt='", alt, "' src='", uri, "'>"] : renderImgs t
  renderImgs [] = []
toStr (Quote str) = L.concat ["<blockquote><p>", str, "</p></blockquote>"]
toStr (Paragraph str) = L.concat ["<p>", str, "</p>"]
toStr (Snippet _ strs) =
  L.concat
    ("<table class='code-snippet'>" :
     contents (1 :: Integer) strs ++ ["</table>"])
  where
    contents line (str:strs') =
      L.concat
        [ "<tr><td class='td-line-num' data-line-num='"
        , L.fromStrict $ Text.pack $ show line
        , "'/><td class='td-content'>"
        , str
        , "</td></tr>"
        ] :
      contents (line + 1) strs'
    contents _ [] = []

convert :: L.Text -> Maybe L.Text
-- ^ Convert markdown to html
convert markdown = toHtml <$> parse markdown

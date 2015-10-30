module Manicure.Markdown where

import qualified Data.ByteString.Char8            as BS
import qualified Manicure.Parser                  as P

import Control.Applicative ((*>), (<*), (<$>), (<*>), (<|>))

data Markdown = Markdown [Item]
          deriving (Eq, Show)
data Item = H5 BS.ByteString | H4 BS.ByteString
          | H3 BS.ByteString | H2 BS.ByteString
          | H1 BS.ByteString
          | Quote BS.ByteString
          | Paragraph BS.ByteString 
          deriving (Eq, Show)

parse :: BS.ByteString -> Markdown
-- ^ Parse the given bytestring
parse str = case P.parseOnly parse_markdown str of
    Right val -> val
    Left err  -> undefined

parse_item :: P.Parser Item
-- ^ The subparser
parse_item = (parse_header <|> parse_quote <|> parse_paragraph) <* P.many1 (P.char '\n')
  where
    parse_header = do
        sharps <- P.try (P.many1 (P.char '#')) <* P.spaces
        rest <- P.noneOf1 "\n"
        return $ case length sharps of
            1 -> H1 rest
            2 -> H2 rest
            3 -> H3 rest
            4 -> H4 rest
            5 -> H5 rest
    parse_quote = do
        P.try (P.char '>') <* P.spaces
        rest <- P.noneOf1 "\n"
        return $ Quote rest
    parse_paragraph = do
        rest <- P.noneOf1 "\n"
        return $ Paragraph rest

parse_markdown :: P.Parser Markdown
-- ^ The actual parser
parse_markdown = do
    items <- P.many1 parse_item
    return $ Markdown items

to_html :: Markdown -> BS.ByteString
-- ^ Generate html
to_html md = ""

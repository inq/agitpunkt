{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE EmptyCase         #-}
module Manicure.Html where

import qualified Data.ByteString.Char8          as BS
import qualified Language.Haskell.TH.Quote      as TQ
import qualified Language.Haskell.TH.Syntax     as TS
import qualified Text.Parsec                    as P
import qualified Text.Parsec.String             as PS
import qualified Text.Parsec.Combinator         as PC
import Control.Applicative ((*>), (<*))
import Text.Parsec ((<|>))

data Node = Tag String [Node]
          | Text String
          deriving Show
data RNode = RTag BS.ByteString [RNode]
          | RText BS.ByteString
          deriving Show
data Status = Child | Sibling | Parent
          deriving Show

instance TS.Lift Node where
    lift (Tag string nodes) = 
        [| RTag (BS.pack string) nodes |]
    lift (Text a) = 
        [| RText (BS.pack a) |]

instance TS.Lift Status where
    lift Child = [| Child |]
    lift Sibling = [| Sibling |]
    lift Parent = [| Parent |]

render :: RNode -> BS.ByteString
render (RTag name nodes) =
    BS.concat [
        "<", name, ">",
        BS.concat $ map render nodes,
        "</", name, ">"
      ]
render (RText text) = text

parseFile :: FilePath -> TS.Q TS.Exp
parseFile file_path = do
     TS.qAddDependentFile file_path
     s <- TS.qRunIO $ readFile file_path
     TQ.quoteExp parse s

parse :: TQ.QuasiQuoter
parse = TQ.QuasiQuoter {
        TQ.quoteExp = quote_exp,
        TQ.quotePat = undefined,
        TQ.quoteType = undefined,
        TQ.quoteDec = undefined
    }
  where
    quote_exp str = do
        filename <- fmap TS.loc_filename TS.location
        case P.parse parseNode filename str of
            Left err -> undefined
            Right tag -> [| tag |]

parseLine :: PS.Parser (Int, Node)
parseLine = do
    next_indent <- parseIndent
    tag <- text_node <|> tag_node
    P.try $ P.string "\n"
    return (next_indent, tag)
  where
    status indent next_indent
        | indent > next_indent = Child
        | indent < next_indent = Parent
        | otherwise = Sibling
    text_node = do
        P.try $ P.string "| "
        res <- P.many $ P.noneOf "\n"
        return $ Text res
    tag_node = do
        res <- P.many $ P.noneOf "\n"
        return $ Tag res []

parseIndent :: PS.Parser Int
parseIndent = do
    indent <- fmap sum $ P.many (
        (P.char ' ' >> return 1) <|> 
        (P.char '\t' >> fail "tab charactor is not allowed")
      )
    return indent

buildTree :: [(Int, Node)] -> (Int, [Node], [(Int, Node)])
buildTree ((now, Tag name arg) : rest)
    | now < next = buildTree $ (now, Tag name res) : remaining
    | now > next = (now, [Tag name arg], rest)
    | otherwise  = (now, (Tag name arg) : res, remaining)
  where
    (next, res, remaining) = buildTree rest
buildTree ((now, Text name) : rest)
    | now < next = error "indentation error"
    | now > next = (now, [Text name], rest)
    | otherwise  = (now, (Text name) : res, remaining)
  where
    (next, res, remaining) = buildTree rest
buildTree []  = 
    (0, [], [])

parseNode :: PS.Parser [Node]
parseNode = do
    nodes <- P.many parseLine
    let (_, res, _) = buildTree nodes
    return $ res

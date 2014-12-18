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
import qualified Manicure.ByteString            as ByteString
import Control.Applicative ((*>), (<*))
import Text.Parsec ((<|>))

data Node = Tag String [Node]
          | Text String
          | Value String
          | Foreach String String [Node]
          deriving Show
data Status = Child | Sibling | Parent
          deriving Show

instance TS.Lift Node where
    lift (Tag string nodes) = 
        [| BS.concat $ 
            ["<", string, ">"] ++ 
            nodes ++ 
            ["</", string, ">"] 
        |]
    lift (Text a) = 
        [| BS.pack a |]
    lift (Value a) = 
        [| ByteString.convert $(return $ TS.VarE $ TS.mkName a) |]
    lift (Foreach vals val nodes) = 
        [| BS.concat $ 
            map
                (\($(return $ TS.VarP $ TS.mkName val)) -> BS.concat nodes)  
                $(return $ TS.VarE $ TS.mkName vals) 
        |]

instance TS.Lift Status where
    lift Child = [| Child |]
    lift Sibling = [| Sibling |]
    lift Parent = [| Parent |]

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
    tag <- value_node <|> text_node <|> map_node <|> tag_node
    P.try $ P.string "\n"
    return (next_indent, tag)
  where
    status indent next_indent
        | indent > next_indent = Child
        | indent < next_indent = Parent
        | otherwise = Sibling
    value_node = do
        P.try $ P.string "= "
        res <- P.many $ P.noneOf "\n"
        return $ Value res
    text_node = do
        P.try $ P.string "| "
        res <- P.many $ P.noneOf "\n"
        return $ Text res
    map_node = do
        P.try $ P.string "- foreach "
        vals <- P.many $ P.noneOf " "
        P.try $ P.string " -> "
        val <- P.many $ P.noneOf "\n"
        return $ Foreach vals val []
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
buildTree ((now, Foreach vals val arg) : rest)
    | now < next = buildTree $ (now, Foreach vals val res) : remaining
    | now > next = (now, [Foreach vals val arg], rest)
    | otherwise  = (now, (Foreach vals val arg) : res, remaining)
  where
    (next, res, remaining) = buildTree rest
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
buildTree ((now, Value name) : rest)
    | now < next = error "indentation error"
    | now > next = (now, [Value name], rest)
    | otherwise  = (now, (Value name) : res, remaining)
  where
    (next, res, remaining) = buildTree rest
buildTree []  = 
    (0, [], [])

parseNode :: PS.Parser [Node]
parseNode = do
    nodes <- P.many parseLine
    let (_, res, _) = buildTree nodes
    return $ res

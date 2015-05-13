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
import qualified Data.List                      as DL
import qualified Data.ByteString.UTF8           as UTF8
import Control.Applicative ((*>), (<*), (<$>), (<*>))
import Text.Parsec ((<|>))

data Attr = Dash String [Attr]
          | Attr String String
          deriving Show
data Node = Tag String [Attr] [Node]
          | Text String
          | Value String
          | Foreach String String [Node]
          deriving Show
data Status = Child | Sibling | Parent
          deriving Show

instance TS.Lift Node where
    lift (Tag string attrs nodes) = [| 
          BS.concat ([$(TS.lift $ "<" ++ string ++ print attrs ++ ">")] ++
            $(TS.lift nodes) ++ 
            [$(TS.lift $ "</" ++ string ++ ">")]
          )
        |]
      where
        print (Attr name value : attrs) = 
            " " ++ name ++ "=" ++ value ++ print attrs
        print [] = ""
    lift (Foreach vals val nodes) = [|
          BS.concat $ 
            map
              (\($(return $ TS.VarP $ TS.mkName val)) -> BS.concat nodes)  
              $(return $ TS.VarE $ TS.mkName vals) 
        |]
    lift (Text a) = [| UTF8.fromString a |]
    lift (Value a) = [| ByteString.convert $(return $ TS.VarE $ TS.mkName a) |]

instance TS.Lift Status where
    lift Child   = [| Child |]
    lift Sibling = [| Sibling |]
    lift Parent  = [| Parent |]

parseFile :: FilePath -> TS.Q TS.Exp
parseFile file_path = do
    TS.qAddDependentFile path
    s <- TS.qRunIO $ readFile path
    TQ.quoteExp parse s
  where
    path = "src/" ++ file_path

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

token :: Char -> PS.Parser (Char)
token c = P.spaces *> P.char c <* P.spaces

parseLine :: PS.Parser (Int, Node)
parseLine = do
    next_indent <- parseIndent
    tag <- value_node <|> text_node <|> map_node <|> tag_node
    P.char '\n'
    return (next_indent, tag)
  where
    status indent next_indent
        | indent > next_indent = Child
        | indent < next_indent = Parent
        | otherwise = Sibling
    tag_node = Tag
        <$> (P.many $ P.noneOf " \n")
        <*> (P.try parse_args <|> return [])
        <*> return []
      where
        parse_args = token '{' *> (PC.sepBy parse_arg $ token ',') <* P.char '}'
        parse_arg = Attr
            <$> parse_until " :"
            <*> (token ':' *> (P.many1 $ P.noneOf ",}"))
    map_node = Foreach
        <$> (P.string "- foreach " *> (P.many $ P.noneOf " ")) 
        <*> (P.string " -> " *> (P.many $ P.noneOf "\n"))
        <*> return []
    value_node = Value <$> (P.string "= " *> (P.many $ P.noneOf "\n"))
    text_node = Text <$> (P.string "| " *> (P.many $ P.noneOf "\n"))
    parse_until str = P.many1 $ P.noneOf str

parseIndent :: PS.Parser Int
parseIndent = fmap sum $ P.many (
        (P.char ' ' >> return 1) <|> 
        (P.char '\t' >> fail "tab charactor is not allowed")
      )

buildTree :: [(Int, Node)] -> (Int, [Node], [(Int, Node)])
buildTree ((indent, node) : rest)
    | indent < next = buildTree $ (indent, replace node res) : remaining
    | indent > next = (indent, [node], rest)
    | otherwise  = (indent, (node) : res, remaining)
  where
    (next, res, remaining) = buildTree rest
    replace (Foreach vals val _) res = Foreach vals val res
    replace (Tag name attr _) res    = Tag name attr res
    replace (Text _) res = error "indentation error"
    replace (Value _) res = error "indentation error"
buildTree []  = 
    (0, [], [])

parseNode :: PS.Parser Node
parseNode = do
    nodes <- P.many parseLine
    let (_, res, _) = buildTree nodes
    return $ head res

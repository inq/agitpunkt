{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
module Manicure.Route (
  Routes,
  parse,
  parseFile,
  match,
) where

import qualified Data.ByteString.Char8          as BS
import qualified Language.Haskell.TH.Quote      as TQ
import qualified Language.Haskell.TH.Syntax     as TS
import qualified Text.Parsec                    as P
import qualified Text.Parsec.String             as PS
import qualified Text.Parsec.Combinator         as PC
import qualified Data.String                    as S
import qualified Manicure.Request               as Req
import qualified Manicure.Response              as Res
import qualified Data.Map.Strict                as M
import Control.Applicative ((*>), (<*))
import Data.Map.Strict ((!))

data Routes = Routes [Route]
data Route = Route String Req.Method String
data RouteTree = Node (M.Map BS.ByteString RouteTree) (M.Map Req.Method Res.Handler)
    deriving Show

instance TS.Lift Route where
    lift (Route uri method action) = [|
            makeNode uri_tokens method $(return $ TS.VarE $ TS.mkName action)
        |]
      where
        split _ [] "" = []
        split _ [] r = [r]
        split c (head : tail) ""
            | head == c    = split c tail ""
            | otherwise    = split c tail [head]
        split c (head : tail) r
            | head == c    = r : split c tail ""
            | otherwise    = split c tail (r ++ [head])
        uri_tokens = filter (not . (== "")) $ split '/' uri ""
instance TS.Lift Routes where
    lift (Routes a) = 
        [| foldl1 mergeNode a |]

mergeNode :: RouteTree -> RouteTree -> RouteTree
-- ^ Merge two nodes
mergeNode (Node a ha) (Node b hb) =
    Node (M.unionWith mergeNode a b) (M.union ha hb)

makeNode :: [BS.ByteString] -> Req.Method -> Res.Handler -> RouteTree
-- ^ Parsing the given ByteStrings, make a route chain
makeNode (head : tail) method action = 
    Node (M.singleton head $ makeNode tail method action) M.empty
makeNode [] method action =
    Node M.empty $ M.singleton method action

match :: BS.ByteString -> Req.Method -> RouteTree -> Res.Action
-- ^ Find a corresponding route from the given request URI
match uri method tree =
    res $ reverse args
  where
    res = map ! method :: Res.Handler
    (Node _ map, args) = find_node uri_tokens tree []
    uri_tokens = filter (not . BS.null) $ BS.split '/' uri
    find_node (head : tail) (Node children _) args = 
        case M.lookup head children of
            Just a  -> find_node tail a args
            Nothing -> find_node tail (children ! "#String") (head : args)
    find_node [] node args = (node, args)
    
parseFile :: FilePath -> TS.Q TS.Exp
-- ^ Parse the route definition file
parseFile file_path = do
     TS.qAddDependentFile file_path
     s <- TS.qRunIO $ readFile file_path
     TQ.quoteExp parse s

parse :: TQ.QuasiQuoter
-- ^ A QuasiQuoter for parsing the route definition
parse = TQ.QuasiQuoter {
        TQ.quoteExp = quote_exp,
        TQ.quotePat = undefined,
        TQ.quoteType = undefined,
        TQ.quoteDec = undefined
    }
  where
    quote_exp str = do
        filename <- fmap TS.loc_filename TS.location
        case P.parse routesNode filename str of
            Left err -> undefined
            Right tag -> [| tag |]

routeNode :: PS.Parser Route
-- ^ The subparser
routeNode = do
    P.many $ P.char '\n'
    uri <- PC.many1 $ P.satisfy (/=' ')
    P.many1 $ P.char ' '
    method <- PC.many1 $ P.satisfy (/=' ')
    P.many1 $ P.char ' '
    action <- PC.many1 $ P.satisfy (/='\n')
    P.many $ P.char '\n'
    return $ Route uri (Req.strToMethod method) action

routesNode :: PS.Parser Routes
-- ^ The main parser
routesNode = do
    lines <- P.many routeNode
    return $ Routes lines

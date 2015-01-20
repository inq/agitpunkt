{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances       #-}
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
import qualified Manicure.Database              as DB
import qualified Data.Map.Strict                as M
import Control.Applicative ((*>), (<*))
import Data.Map.Strict ((!))

type Handler = DB.Connection -> Req.Request -> IO Res.Response

data Routes = Routes [Route]
data Route = Route String Req.Method String
data RouteTree = Node (M.Map BS.ByteString RouteTree) (M.Map Req.Method Handler)
                 deriving Show

instance Show Handler where
    show _ = "Handler"

instance TS.Lift Route where
    lift (Route uri method action) = [|
            let uri_tokens = filter (not . BS.null) $ BS.split '/' uri
            in makeNode uri_tokens method $(return $ TS.VarE $ TS.mkName action)
        |]
instance TS.Lift Routes where
    lift (Routes a) = 
        [| foldl1 mergeNode a |]

mergeNode :: RouteTree -> RouteTree -> RouteTree
mergeNode (Node a ha) (Node b hb) =
    Node (M.unionWith mergeNode a b) (M.union ha hb)

makeNode :: [BS.ByteString] -> Req.Method -> Handler -> RouteTree
makeNode (head : tail) method action = 
    Node (M.singleton head $ makeNode tail method action) M.empty
makeNode [] method action =     
    Node M.empty $ M.singleton method action

match :: BS.ByteString -> Req.Method -> RouteTree -> DB.Connection -> Req.Request -> IO Res.Response
match uri method tree =
    map ! method
  where
    Node _ map = find_node uri_tokens tree
    uri_tokens = filter (not . BS.null) $ BS.split '/' uri
    find_node (head : tail) (Node children _) = children ! head
    find_node [] node = node
    

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
        case P.parse routesNode filename str of
            Left err -> undefined
            Right tag -> [| tag |]

routeNode :: PS.Parser Route
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
routesNode = do
    lines <- P.many routeNode
    return $ Routes lines

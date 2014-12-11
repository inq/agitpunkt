{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
module Manicure.Route (
  Routes,
  parse,
  parseFile,
  extract
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
import Control.Applicative ((*>), (<*))

data Routes = Routes [Route]
data Route = Route String Req.Method (DB.Connection -> Req.Request -> IO Res.Response)
           | RouteR String Req.Method String

instance TS.Lift Route where
    lift (RouteR uri method action) = 
        [| Route uri method $(return $ TS.VarE $ TS.mkName action) |]
instance TS.Lift Routes where
    lift (Routes a) = 
        [| Routes a |]

extract :: Routes -> DB.Connection -> Req.Request -> IO Res.Response
extract (Routes routes) =
   action
  where
    Route _ _ action = head routes

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
    return $ RouteR uri (Req.strToMethod method) action

routesNode :: PS.Parser Routes
routesNode = do
    lines <- P.many routeNode
    return $ Routes lines

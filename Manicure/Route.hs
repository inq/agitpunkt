{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
module Manicure.Route (
  Routes,
  parse,
  parseFile,
  extract
) where

import qualified Data.ByteString.Char8      as BS
import qualified Language.Haskell.TH.Quote  as TQ
import qualified Language.Haskell.TH.Syntax as TS
import qualified Text.Parsec                as P
import qualified Text.Parsec.String         as PS
import qualified Text.Parsec.Combinator     as PC
import qualified Manicure.Request           as Request
import qualified Data.String                as S
import Control.Applicative ((*>), (<*))

data Routes = Routes [Route]
    deriving Show
data Route = Route String Request.Method String
    deriving Show

instance TS.Lift Route where
    lift (Route uri method action) = [| Route uri method action |]
instance TS.Lift Routes where
    lift (Routes a) = [| Routes a |]

extract :: Routes -> String
extract (Routes routes) =
    location
  where
    Route _ _ location = head routes

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
    return $ Route uri (Request.strToMethod method) action

routesNode :: PS.Parser Routes
routesNode = do
    lines <- P.many routeNode
    return $ Routes lines

installRoutes action =
    TS.VarE $ TS.mkName action

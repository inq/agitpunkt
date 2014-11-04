{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Manicure.Route (
  Routes,
  parse
) where

import qualified Data.ByteString.Char8      as BS
import qualified Language.Haskell.TH.Quote  as TQ
import qualified Language.Haskell.TH.Syntax as TS
import qualified Text.Parsec                as P
import qualified Text.Parsec.String         as PS
import qualified Text.Parsec.Combinator     as PC
import qualified Manicure.Request           as Request
import Control.Applicative ((*>), (<*))

data Routes = Routes [Route]
    deriving Show
data Route = Route String
    deriving Show

instance TS.Lift Route where
    lift (Route a) = [| Route a |]
instance TS.Lift Routes where
    lift (Routes a) = [| Routes a |]

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
    line <- (P.many $ P.char '\n') *> (PC.many1 $ P.satisfy (/='\n')) <* (P.many $ P.char '\n')
    return $ Route line

routesNode :: PS.Parser Routes
routesNode = do
    lines <- P.many routeNode
    return $ Routes lines

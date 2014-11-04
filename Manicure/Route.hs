{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Manicure.Route (
  route,
  parseRoutes,
  RouteSetting
) where

import qualified Data.ByteString.Char8      as BS
import qualified Language.Haskell.TH.Quote  as TQ
import qualified Language.Haskell.TH.Syntax as TS
import qualified Text.Parsec                as P
import qualified Text.Parsec.String         as PS
import qualified Text.Parsec.Combinator     as PC
import qualified Manicure.Request           as Request

data RouteSetting = Text String
    deriving Show

instance TS.Lift RouteSetting where
    lift (Text t) = [| Text t |]

parseRoutes :: TQ.QuasiQuoter
parseRoutes = TQ.QuasiQuoter {
    TQ.quoteExp = routeExpr,
    TQ.quotePat = undefined,
    TQ.quoteType = undefined,
    TQ.quoteDec = undefined
}

routeExpr :: String -> TS.Q TS.Exp
routeExpr str = do
    filename <- TS.loc_filename `fmap` TS.location
    case P.parse routeNode filename str of
      Left err -> undefined
      Right tag -> [| tag |]

routeNode :: PS.Parser RouteSetting
routeNode = fmap Text $ PC.many1 $ P.satisfy (/='\0')

route :: Request.Request -> IO ()
route request = do
    putStr ""

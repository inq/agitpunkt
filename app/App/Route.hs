{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App.Route where

import           App.Component              (Handler)
import           Core.Request               (Method)
import qualified Data.Map.Strict            as M
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Language.Haskell.TH.Quote  (QuasiQuoter (..), quoteExp)
import           Language.Haskell.TH.Syntax (Exp (VarE), Lift (lift), Q, mkName,
                                             qAddDependentFile, qRunIO)
import qualified Misc.Parser                as P

-- * Data types
data Routes =
  Routes ![Route]

data Route =
  Route !String
        !Method
        !String

data RouteTree =
  Node !(M.Map Text RouteTree)
       !(M.Map Method Handler)

-- * Instances
instance Lift Route where
  lift (Route uri method action) =
    [|makeNode uriTokens method $(return $ VarE $ mkName action)|]
    where
      split _ [] "" = []
      split _ [] r = [r]
      split c (char:chars) ""
        | char == c = split c chars ""
        | otherwise = split c chars [char]
      split c (char:chars) r
        | char == c = r : split c chars ""
        | otherwise = split c chars (r ++ [char])
      uriTokens = filter (/= "") $ split '/' uri ""

instance Lift Routes where
  lift (Routes a) = [|foldl1 mergeNode a|]

mergeNode :: RouteTree -> RouteTree -> RouteTree
-- ^ Merge two nodes
mergeNode (Node a ha) (Node b hb) =
  Node (M.unionWith mergeNode a b) (M.union ha hb)

makeNode :: [Text] -> Method -> Handler -> RouteTree
-- ^ Parsing the given ByteStrings, make a route chain
makeNode (str:strs) method action =
  Node (M.singleton str $ makeNode strs method action) M.empty
makeNode [] method action = Node M.empty $ M.singleton method action

match :: Text -> Method -> RouteTree -> Maybe (Handler, [Text])
-- ^ Find a corresponding route from the given request URI
match uri method tree =
  case M.lookup method _map of
    Just res -> Just (res, reverse args)
    Nothing  -> Nothing
  where
    (Node _ _map, args) = findNode uriTokens tree []
    uriTokens = filter (not . Text.null) $ Text.split (== '/') uri
    findNode (_head:_tail) (Node children _) params =
      case M.lookup _head children of
        Just a -> findNode _tail a params
        Nothing ->
          case M.lookup "#String" children of
            Just a  -> findNode _tail a (_head : params)
            Nothing -> (Node M.empty M.empty, [])
    findNode [] node params = (node, params)

parseFile :: FilePath -> Q Exp
-- ^ Parse the route definition file
parseFile filePath = do
  qAddDependentFile filePath
  s <- qRunIO $ readFile filePath
  quoteExp parse s

parse :: QuasiQuoter
-- ^ A QuasiQuoter for parsing the route definition
parse =
  QuasiQuoter
  { quoteExp = qexp
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
  where
    qexp str =
      case P.parseOnly routesNode (Text.pack str) of
        Left _    -> undefined
        Right tag -> [|tag|]

routeNode :: P.Parser Route
-- ^ The subparser
routeNode = do
  uri <- Text.unpack <$> (P.many (P.char '\n') *> P.noneOf1 " ")
  method <- (read . Text.unpack) <$> (P.many1 (P.char ' ') *> P.noneOf1 " ")
  action <-
    Text.unpack <$>
    (P.many (P.char ' ') *> P.noneOf1 "\n" <* P.many (P.char '\n'))
  return $ Route uri method action

routesNode :: P.Parser Routes
-- ^ The main parser
routesNode = Routes <$> P.many routeNode

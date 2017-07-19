{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Base where

import qualified Data.Bson           as Bson
import           Data.Map.Strict     ((!))
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Time           as T
import qualified Data.Time.Format    as TF
import qualified Language.Haskell.TH as TH
import qualified Models.Category     as Ct

data RoseTree = Node
  { _id      :: Maybe Bson.ObjectId
  , name     :: Text
  , children :: [RoseTree]
  } deriving (Show)

data Entry = Entry
  { eId    :: Bson.ObjectId
  , eLevel :: Int
  , eName  :: Text
  }

toStrList :: [RoseTree] -> [Entry]
toStrList = concatMap (toStrList' one)
  where
    one = 1 :: Int
    toStrList' l (Node i n cr) =
      Entry (unMaybe i) l n : concatMap (toStrList' $ l + 1) cr
    unMaybe (Just x) = x
    unMaybe Nothing  = error "No category id"

convert :: [Ct.Category] -> [RoseTree]
convert cs = map toRose roots
  where
    toRose (Ct.Category i n _) = Node i n cr
      where
        cr = map toRose (fromMaybe [] (M.lookup i parentMap))
    roots = parentMap ! Nothing
    parentMap = parentMap' M.empty cs
    parentMap' m (c@(Ct.Category _ _ p):res) = parentMap' m' res
      where
        m' = M.insertWith (++) p [c] m
    parentMap' m [] = m

compiled :: Text
compiled =
  Text.concat
    [ "compiled at "
    , $(TH.stringE =<<
        TH.runIO
          (TF.formatTime TF.defaultTimeLocale "%Y-%m-%d" <$> T.getCurrentTime))
    ]

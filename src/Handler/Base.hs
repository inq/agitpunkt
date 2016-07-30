{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Base where

import qualified Data.ByteString.Char8            as BS
import qualified Language.Haskell.TH              as TH
import qualified Data.Time.Format                 as TF
import qualified Data.Time                        as T
import qualified Data.Map.Strict                  as M
import qualified Models.Category                  as Ct
import qualified Data.Bson                        as Bson
import Data.Map.Strict ((!))

data RoseTree = Node
  { _id :: Maybe Bson.ObjectId
  , name :: BS.ByteString
  , children :: [RoseTree]
  } deriving (Show)

toStrList :: [RoseTree] -> [[BS.ByteString]]
toStrList cs = concat $ map (toStrList' one) cs
  where
    one = 1 :: Int
    toStrList' l (Node i n cr) =
      [n, BS.pack $ unMaybe i, BS.pack ("l" ++ show l)]
        : (concat $ map (toStrList' $ l + 1) cr)
    unMaybe (Just x) = show x
    unMaybe Nothing = ""

convert :: [Ct.Category] -> [RoseTree]
convert cs = map toRose roots
  where
    toRose (Ct.Category i n _) = Node i n cr
      where
        cr = map toRose $ case M.lookup i parentMap of
            Just x -> x
            Nothing -> []
    roots = parentMap ! Nothing
    parentMap = parentMap' M.empty cs
    parentMap' m (c@(Ct.Category _ _ p) : res) = parentMap' m' res
      where
        m' = M.insertWith (++) p [c] m
    parentMap' m [] = m

compiled :: BS.ByteString
compiled = BS.concat
  [ "compiled at "
  , $(TH.stringE
      =<< TH.runIO (TF.formatTime TF.defaultTimeLocale "%Y-%m-%d"
                    <$> T.getCurrentTime)
     )
  ]

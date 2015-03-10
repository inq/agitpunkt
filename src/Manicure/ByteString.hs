{-# LANGUAGE FlexibleInstances #-}
module Manicure.ByteString where

import qualified Data.ByteString.Char8          as BS
import qualified Data.Map                       as M
import qualified Network.HTTP.Types.URI         as URI

class StringFamily a where
    convert :: a -> BS.ByteString
instance StringFamily BS.ByteString where
    convert bs = bs
instance StringFamily String where
    convert str = BS.pack str

type QueryString = M.Map BS.ByteString BS.ByteString

split_and_decode :: BS.ByteString -> QueryString
-- ^ Split the given string and construct the Map
split_and_decode bs = M.fromList $ map transform (BS.split '&' bs)
  where
    transform line = pair
      where
        idx = case BS.elemIndex '=' line of
            Just i -> i
            Nothing -> 0
        pair = (decode $ BS.take idx line, decode $ BS.drop (idx + 1) line)
          where
            decode = URI.urlDecode True

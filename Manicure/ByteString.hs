{-# LANGUAGE FlexibleInstances #-}
module Manicure.ByteString where

import qualified Data.ByteString.Char8          as BS

class StringFamily a where
    convert :: a -> BS.ByteString
instance StringFamily BS.ByteString where
    convert bs = bs
instance StringFamily String where
    convert str = BS.pack str

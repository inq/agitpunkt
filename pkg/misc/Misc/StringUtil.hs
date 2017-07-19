{-# LANGUAGE FlexibleInstances #-}

module Misc.StringUtil (
  toSnake
  ) where

import           Data.Char (isUpper, toLower)

toSnake :: String -> String
toSnake (h:t) = toLower h : internal t
  where
  internal (ih:it) =
    if isUpper ih
    then '_' : toLower ih : internal it
    else ih : internal it
  internal [] = []
toSnake [] = []

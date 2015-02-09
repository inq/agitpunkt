module Manicure.Http where

data Version = Version {
  major :: !Int,
  minor :: !Int
} deriving (Show)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.Exception where

import           Control.Monad
import           Core.Html     (parse)
import qualified Core.Response as Res
import           Data.Map      ((!))

default404 :: Res.Handler
-- ^ 404 page
default404 [] db req =
  return $
  [parse|html
      body
        div
          p
            | oops 404!
     |]
    []

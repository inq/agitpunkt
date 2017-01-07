{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Handler.Exception where

import qualified Core.Response as Res
import Core.Html (parse)
import Control.Monad
import Data.Map ((!))

default404 :: Res.Handler
-- ^ 404 page
default404 [] db req =
    return $ [parse|html
      body
        div
          p
            | oops 404!
     |] []

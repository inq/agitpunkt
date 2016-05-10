{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Exception where

import qualified Core.Response                  as Res
import qualified Core.Html                      as Html
import Control.Monad
import Data.Map ((!))

default404 :: Res.Handler
-- ^ 404 page
default404 [] db req = do
    return $ Res.success $(Html.parseFile "404.html.qh") []

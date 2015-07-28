{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Exception where

import qualified Manicure.Response              as Res
import qualified Manicure.Html                  as Html
import Control.Monad
import Data.Map ((!))

default404 :: Res.Handler
-- ^ 404 page
default404 [] db req = do
    return $ Res.success $(Html.parseFile "Views/404.html.qh") []

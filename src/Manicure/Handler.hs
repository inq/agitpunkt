{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Manicure.Handler where

import qualified Manicure.Route                  as Route
import qualified Manicure.Response               as Res
import qualified Handler.Article                 as Article
import qualified Handler.Main                    as Main
import qualified Handler.Auth                    as Auth

routeTree = $(Route.parseFile "config/routes.cfg")

{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
module Manicure.Handler where

import qualified Manicure.Route                  as Route
import qualified Manicure.Response               as Res
import Handler.Main

route_tree = $(Route.parseFile "config/routes.cfg")

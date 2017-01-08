{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Handler where

import qualified App.Route as Route
import qualified Handler.Article as Article
import qualified Handler.Auth as Auth
import qualified Handler.Image as Image

routeTree :: Route.RouteTree
routeTree = $(Route.parseFile "config/routes.cfg")

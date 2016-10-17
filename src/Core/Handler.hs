{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Handler where

import qualified Core.Route as Route
import qualified Core.Response as Res
import qualified Handler.Article as Article
import qualified Handler.Auth as Auth
import qualified Handler.Image as Image

routeTree = $(Route.parseFile "config/routes.cfg")

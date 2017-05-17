{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.Image
  ( index
  , create
  , view
  ) where

import           App.Component       (Handler, postData, runDB)
import qualified Config
import qualified Core.Response       as Res
import           Handler.Application
import           Misc.Html           (parse)
import qualified Models.Image        as Image

index :: Handler
-- ^ List the images
index = do
  assertUser Config.adminUser
  images <- runDB $ Image.find 10
  html <-
    layout
      [parse|div { class="article" }
    div { class="wrapper" }
      form { action="/image/new", method="post", enctype="multipart/form-data" }
        input { type="file", name="data" }
        input { type="submit" }
    - map images -> image
      div { class="wrapper" }
        img { src=Image.imgUrl image }
  |]
  return $ Res.success html []

create :: Handler
-- ^ Create the image
create = do
  _ <- assertUser Config.adminUser
  d <- postData "data"
  case d of
    Just x -> runDB $ Image.save x
    _      -> return ()
  index

view :: Handler
-- ^ Show the image
view = do
  _ <- assertUser Config.adminUser
  html <-
    layout
      [parse|div { class="article" }
    div { class="wrapper" }
      | hey
  |]
  return $ Res.success html []

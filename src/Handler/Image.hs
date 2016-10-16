{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Handler.Image where

import qualified Core.Response as Res
import qualified Models.Image as Image
import Core.Component (Handler, postData, runDB)
import Misc.Html (parse)
import Handler.Application

index :: Handler
-- ^ List the images
index = do
  assertUser "gofiri@gmail.com"
  images <- runDB Image.find

  html <- layout [parse|div { class="article" }
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
  _ <- assertUser "gofiri@gmail.com"
  d <- postData "data"
  case d of
    Just x -> runDB $ Image.save x
    _ -> return ()

  html <- layout [parse|div { class="article" }
    div { class="wrapper" }
      | Uploaded
  |]
  return $ Res.success html []

show :: Handler
-- ^ List the images
show = do
  _ <- assertUser "gofiri@gmail.com"
  html <- layout [parse|div { class="article" }
    div { class="wrapper" }
      | hey
  |]
  return $ Res.success html []

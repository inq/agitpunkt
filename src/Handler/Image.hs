{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Handler.Image where

import qualified Core.Response as Res
import Core.Component (Handler)
import Core.Html (parse)
import Handler.Application


index :: Handler
-- ^ List the images
index = do
  html <- layout [parse|div { class="article" }
    div { class="wrapper" }
      form { action="/image/new", method="post", enctype="multipart/formdata" }
        input { type="file", name="data" }
        input { type="submit" }
  |]
  return $ Res.success html []

create :: Handler
-- ^ List the images
create = do
  html <- layout [parse|div { class="article" }
    div { class="wrapper" }
      | uploaded!
  |]
  return $ Res.success html []

show :: Handler
-- ^ List the images
show = do
  html <- layout [parse|div { class="article" }
    div { class="wrapper" }
      | hey
  |]
  return $ Res.success html []

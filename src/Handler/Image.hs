{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Handler.Image where

import qualified Core.Response as Res
import Core.Component (Handler, requestHeader, reqm, reqn)
import Misc.Html (parse)
import Handler.Application


index :: Handler
-- ^ List the images
index = do
  html <- layout [parse|div { class="article" }
    div { class="wrapper" }
      form { action="/image/new", method="post", enctype="multipart/form-data" }
        input { type="file", name="data" }
        input { type="submit" }
  |]
  return $ Res.success html []

create :: Handler
-- ^ List the images
create = do
  res <- Prelude.show <$> requestHeader "Content-Type"
  x <- reqm
  y <- reqn
  html <- layout [parse|div { class="article" }
    div { class="wrapper" }
      = x
    div { class="wrapper" }
      = y
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

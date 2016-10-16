{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Handler.Image where

import qualified Core.Response as Res
import qualified Models.Image as Image
import Core.Component (Handler, requestHeader, postData)
import Misc.Html (parse)
import Handler.Application
import Control.Monad.State (liftIO)

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
  d <- postData "data"
  liftIO $ case d of
    Just x -> Image.save x
    _ -> return ()
  let file = Prelude.show d

  html <- layout [parse|div { class="article" }
    div { class="wrapper" }
      $ file
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

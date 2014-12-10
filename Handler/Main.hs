{-# LANGUAGE OverloadedStrings #-}
module Handler.Main where

import qualified Data.ByteString.Char8          as BS
import qualified Manicure.Request               as Req
import qualified Manicure.Response              as Res

index :: Req.Request -> Res.Response
index val = Res.success $ BS.pack $ show val

post_test :: Req.Request -> Res.Response
post_test val = Res.success "post test"

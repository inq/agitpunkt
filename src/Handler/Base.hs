{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Base where

import qualified Data.ByteString.Char8          as BS
import qualified Language.Haskell.TH            as TH
import qualified Data.Time.Format               as TF
import qualified Data.Time                      as T

compiled :: BS.ByteString
compiled = BS.concat
  [ "compiled at "
  , $(TH.stringE
      =<< TH.runIO (TF.formatTime TF.defaultTimeLocale "%Y-%m-%d"
                    <$> T.getCurrentTime)
     )
  ]

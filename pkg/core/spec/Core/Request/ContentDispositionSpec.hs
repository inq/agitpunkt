{-# LANGUAGE OverloadedStrings #-}

module Core.Request.ContentDispositionSpec where

import           Core.Request.ContentDisposition
import qualified Misc.Parser                     as P
import           SpecHelper

spec :: Spec
spec =
  describe "Core.Request.ContentSpec" $ 
    context "Simple parsing" $
      it "parses Content-Disposition" $ do
        let contDisp = "form-data; name=\"data\"; filename=\"theFile.png\""
        P.parseOnly parse contDisp `shouldBe`
          Right (ContentDisposition FormData (Just "data") (Just "theFile.png"))

main :: IO ()
main = hspec spec

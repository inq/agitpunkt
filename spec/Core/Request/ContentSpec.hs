{-# LANGUAGE OverloadedStrings #-}

module Core.Request.ContentSpec where

import           Core.Request.Content
import qualified Data.ByteString        as BS
import qualified Data.Map               as M
import qualified Misc.Parser            as P
import qualified Misc.Parser.ByteString as BSParser
import           SpecHelper

spec :: Spec
spec =
  describe "Core.Request.ContentSpec" $
  context "Simple parsing" $
  it "parses multipart data" $ do
    let contDisp =
          BS.concat
            [ "--------BOUNDARY\r\n"
            , "Content-Disposition: form-data; name=\"data\"; filename=\"theFile.png\"\r\n"
            , "Content-Type: jpeg\r\n"
            , "\r\n"
            , "TheContent\r\n"
            , "--------BOUNDARY\r\n"
            ]
    BSParser.parseOnly (parseMultipart "------BOUNDARY") contDisp `shouldBe`
      Right
        (M.fromList
           [("data", MkFile (Just "theFile.png") (Just "jpeg") "TheContent")])

main :: IO ()
main = hspec spec

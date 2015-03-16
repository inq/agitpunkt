{-# LANGUAGE OverloadedStrings #-}
module Manicure.JsonSpec where

import qualified Data.ByteString.Char8       as BS
import qualified Data.Map.Strict                as M
import Manicure.Json
import SpecHelper


spec :: Spec
spec = do
    describe "Manicure.JsonSpec" $ do
        context "Simple Text" $ do
            it "parses simple string" $ do
                 parse " \"Sample String\" " `shouldBe`
                     JSString "Sample String"
            it "parses simple array" $ do
                 parse " [ \"1\",\"2\"\t, \"3\" ]" `shouldBe` 
                     JSArray [JSString "1", JSString "2", JSString "3"]
            it "parses simple object" $ do
                 parse " { \"key\":[\"value\"] } " `shouldBe`
                     JSObject (M.fromList [("key", JSArray [JSString "value"])])
main :: IO ()
main = hspec spec

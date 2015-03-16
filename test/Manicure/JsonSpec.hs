{-# LANGUAGE OverloadedStrings #-}
module Manicure.JsonSpec where

import qualified Data.ByteString.Char8       as BS
import Manicure.Json
import SpecHelper


spec :: Spec
spec = do
    describe "Manicure.JsonSpec" $ do
        context "Simple Text" $ do
            it "parses simple string" $ do
                 parse " \"Hello\" " `shouldBe` JSString "Hello"
main :: IO ()
main = hspec spec

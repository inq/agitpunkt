{-# LANGUAGE OverloadedStrings #-}

module Misc.TextUtilSpec where

import qualified Data.Map.Strict as M
import           Misc.TextUtil
import           SpecHelper

spec :: Spec
spec =
  describe "Misc.TextUtilSpec" $
  context "Simple Text" $ do
    it "parses simple string" $
      splitAndDecode '&' " hello = hihi" `shouldBe`
      M.fromList [("hello", "hihi")]
    it "parses url-encoded string" $
      splitAndDecode '&' " a%20a=  %30hi" `shouldBe` M.fromList [("a a", "0hi")]
    it "parses three arguments" $
      splitAndDecode ';' " hello=hihi;a     =b; c = e" `shouldBe`
      M.fromList [("hello", "hihi"), ("a", "b"), ("c", "e")]

main :: IO ()
main = hspec spec

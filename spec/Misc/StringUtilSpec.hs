{-# LANGUAGE OverloadedStrings #-}

module Misc.StringUtilSpec where

import           Misc.StringUtil
import           SpecHelper

spec :: Spec
spec =
  describe "Misc.StringUtilSpec" $
  context "Simple string" $ do
    it "convert to snake case" $ do
      toSnake "AAA" `shouldBe` "a_a_a"
      toSnake "AaA" `shouldBe` "aa_a"
      toSnake "Aaa" `shouldBe` "aaa"

main :: IO ()
main = hspec spec

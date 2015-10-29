{-# LANGUAGE OverloadedStrings #-}
module Manicure.MarkdownSpec where

import qualified Data.ByteString.Char8       as BS
import qualified Data.Map.Strict                as M
import Manicure.Markdown
import SpecHelper


spec :: Spec
spec = do
    describe "Manicure.MarkdownSpec" $ do
        context "Simple Text" $ do
            it "parses simple string" $ do
                parse "#### Hello\n\n" `shouldBe`
                    Markdown [H4 "Hello"]

main :: IO ()
main = hspec spec

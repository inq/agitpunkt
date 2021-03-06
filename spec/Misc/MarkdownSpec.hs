{-# LANGUAGE OverloadedStrings #-}

module Misc.MarkdownSpec where

import           Misc.Markdown
import           SpecHelper

spec :: Spec
spec =
  describe "Core.MarkdownSpec" $ do
    context "Simple Text" $ do
      it "parses simple string" $ do
        parse "#### Hello" `shouldBe` Just (Markdown [H4 "Hello"])
        parse "! hi-hihi; /img/img.png\r\n" `shouldBe`
          Just (Markdown [Container [Img "hi-hihi" "/img/img.png"]])
        parse "Hello\r\n\r\nHihi\r\n" `shouldBe`
          Just (Markdown [Paragraph "Hello", Paragraph "Hihi"])
      it "parses simple enippet" $
        parse "Hello\r\n```rust\r\nHELLO\r\nWorld\r\n```\r\n" `shouldBe`
        Just (Markdown [Paragraph "Hello", Snippet "rust" ["HELLO", "World"]])
    context "Multiple Images" $ 
      it "renders multiple images" $
        parse
          "{{{\r\n! hi-hihi; /img/img.png\r\n\r\n\r\n! hi-hihi; /img/img.png\r\n! hi-hihi; /img/img.png\r\n}}}\r\n" `shouldBe`
          Just
            (Markdown
               [ Container
                   [ Img "hi-hihi" "/img/img.png"
                   , Img "hi-hihi" "/img/img.png"
                   , Img "hi-hihi" "/img/img.png"
                   ]
               ])
    context "To Html" $ do
      it "generate h4 string" $
        toHtml (Markdown [Paragraph "HI"]) `shouldBe` "<p>HI</p>"
      it "generate img tag" $
        toHtml (Markdown [Container [Img "hi-hi" "/img/img.png"]]) `shouldBe`
        "<div class='paragraph1'><img class='content-img' alt='hi-hi' src='/img/img.png'></div>"
      it "generate paragraphs" $
        toHtml (Markdown [Paragraph "HI", Paragraph "Hello"]) `shouldBe`
        "<p>HI</p><p>Hello</p>"

main :: IO ()
main = hspec spec

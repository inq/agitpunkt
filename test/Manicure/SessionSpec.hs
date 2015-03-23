{-# LANGUAGE OverloadedStrings #-}
module Manicure.SessionSpec where

import qualified Data.ByteString.Char8       as BS
import qualified Data.Map.Strict                as M
import Manicure.Session
import SpecHelper


spec :: Spec
spec = do
    describe "Manicure.SessionSpec" $ do
        context "Simple Text" $ do
            it "parses simple string" $ do
                 putStrLn "HELLO"

main :: IO ()
main = hspec spec

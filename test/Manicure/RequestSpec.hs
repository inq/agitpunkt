{-# LANGUAGE OverloadedStrings #-}
module Manicure.RequestSpec where

import qualified Data.ByteString.Char8          as BS
import qualified Manicure.Http                  as Http
import qualified Data.Char                      as C
import qualified Data.Either                    as E
import qualified Manicure.ByteString            as ByteString
import qualified Data.Attoparsec.ByteString     as AB
import qualified Data.Attoparsec.ByteString.Char8 as AC
import Data.Word (Word8)
import Control.Applicative ((*>), (<*), (<$>), (<*>), many)
import qualified Data.Map.Strict                as M
import Manicure.Request
import SpecHelper
import System.IO


spec :: Spec
spec = do
    describe "Manicure.RequestSpec" $ do
        context "Simple Text" $ do
            it "parses simple header" $ do
                BS.putStrLn head
              where
                head = case AC.parseOnly request "GET / HTTP/1.1\r\nHello: hihi\r\n\r\n" of
                   Right res  -> res
                   Left  str -> error str
                isToken w = w <= 127 && AB.notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w
                request = AC.takeByteString

                header = (,)
                    <$> (AB.takeWhile isToken <* AC.char8 ':' <* AB.skipWhile AC.isHorizontalSpace)
                    <*> (AB.takeTill AC.isEndOfLine <* AC.endOfLine)

main :: IO ()
main = hspec spec

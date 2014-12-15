{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Main where

import qualified Data.ByteString.Char8          as BS
import qualified Manicure.Request               as Req
import qualified Manicure.Response              as Res
import qualified Manicure.Database              as DB
import qualified Database.MongoDB               as M
import qualified Data.Text                      as T
import qualified Manicure.Html                  as Html

extract :: [M.Document] -> T.Text -> IO [String]
extract documents key = mapM read documents
  where
    read document = M.lookup key document

index :: DB.Connection -> Req.Request -> IO Res.Response
index db req = do
    articles <- DB.query db DB.find
    titles <- extract articles "title"
    let testData = BS.concat $ map wrap titles
    return $ Res.success $ Html.render $ head $(Html.parseFile "Views/index.html.qh") 
  where
    wrap title = BS.concat [
        "<div>",
        BS.pack title,
        "</div>"
      ]


post_test :: DB.Connection -> Req.Request -> IO Res.Response
post_test db val = do
    return $ Res.success "post test"

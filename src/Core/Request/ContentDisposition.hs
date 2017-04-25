{-# LANGUAGE OverloadedStrings #-}

module Core.Request.ContentDisposition where

import           Data.Char   (toLower)
import qualified Data.Map    as M
import           Data.Text   (Text)
import qualified Data.Text   as Text
import qualified Misc.Parser as P

-- * Data types
data ContentDisposition = ContentDisposition
  { dispType :: Type
  , name     :: Maybe Text
  , filename :: Maybe Text
  } deriving (Eq, Show)

data Type
  = Inline
  | Attachment
  | FormData
  deriving (Eq, Show)

-- * Parser
parse :: P.Parser ContentDisposition
-- ^ https://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html
parse = do
  dtypebs <- Text.map toLower <$> P.takeTill' (== ';') <* P.char ';'
  dtype <-
    case dtypebs of
      "form-data"  -> return FormData
      "inline"     -> return Inline
      "attachment" -> return Attachment
      _            -> fail "Invalid ContentDisposition"
  rmap <-
    M.fromList <$>
    P.sepBy
      ((,) <$> (P.spaces *> P.noneOf "= " <* P.spaces <* P.char '=') <*>
       (P.spaces *> P.quoted))
      (P.char ';')
  return $
    ContentDisposition dtype (M.lookup "name" rmap) (M.lookup "filename" rmap)

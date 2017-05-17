{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Image where

import           Codec.Picture        (DynamicImage (..),
                                       PixelRGBA8 (PixelRGBA8), convertRGBA8,
                                       decodeImage, generateImage, imageHeight,
                                       imageWidth, pixelAt, saveJpgImage,
                                       savePngImage)
import qualified Codec.Picture        as J
import           Control.Arrow        ((&&&), (***))
import           Control.Monad        (join)
import           Control.Monad.State  (liftIO)
import           Core.Request.Content (Context (..))
import           Data.Bson            (genObjectId, (!?), (=:))
import qualified Data.Bson            as Bson
import qualified Data.ByteString      as BS
import           Data.Ratio           ((%))
import           Data.Text            (Text, pack)
import qualified Data.Text            as Text
import qualified Data.Time.Clock      as TC
import           Data.Time.Format     (defaultTimeLocale, formatTime)
import qualified Database.MongoDB     as Mongo
import           GHC.Word             (Word32)
import           System.Directory     (createDirectoryIfMissing)

data Image = Image
  { _id       :: Maybe Bson.ObjectId
  , origFile  :: Text
  , createdAt :: TC.UTCTime
  } deriving (Show)

data Extension
  = PNG
  | JPG
  deriving (Eq)

toText :: Extension -> Text
toText PNG = ".png"
toText JPG = ".jpg"

getExtension :: Text -> Extension
-- ^ TODO: Support gif
getExtension fname =
  case (Text.toLower $ Text.takeEnd 4 fname) of
    ".png" -> PNG
    _      -> JPG

getDirectory :: TC.UTCTime -> String
getDirectory = formatTime defaultTimeLocale "%y%m%d"

imgUrl :: Image -> Text
imgUrl (Image (Just id') origFile' created') =
  Text.concat
    [ "/static/data/"
    , pack $ getDirectory created'
    , "/"
    , pack $ show id'
    , "/900"
    , (toText . getExtension) origFile'
    ]
imgUrl _ = "Unreachable"

find :: Word32 -> Mongo.Action IO [Image]
-- ^ Find images
find limit = do
  res <-
    Mongo.find
      (Mongo.select [] "images")
      {Mongo.sort = ["_id" =: (1 :: Int)], Mongo.limit = limit} >>=
    Mongo.rest
  return $ map fromDocument res
  where
    fromDocument doc =
      Image
      { _id = doc !? "_id"
      , origFile = Bson.at "origFile" doc
      , createdAt = Bson.at "createdAt" doc
      }

save :: Context -> Mongo.Action IO ()
-- ^ Save the image file
save (MkFile (Just fname) _ d) = do
  let ext = (toText . getExtension) fname
  current <- liftIO TC.getCurrentTime
  objId <- liftIO genObjectId
  let parentDir = "data/" ++ getDirectory current
      imgDir = parentDir ++ "/" ++ show objId
  liftIO $ createDirectoryIfMissing False parentDir
  liftIO $ createDirectoryIfMissing False imgDir
  case decodeImage d of
    Right i -> do
      let img = convertRGBA8 i
      let fact = 900 % imageWidth img
      let img' =
            if fact > 1
              then ImageRGBA8 img
              else ImageRGBA8 $ resize4 fact img
      liftIO $
        if getExtension fname == PNG
          then savePngImage (imgDir ++ "/900" ++ Text.unpack ext) img'
          else saveJpgImage 100 (imgDir ++ "/900" ++ Text.unpack ext) img'
    _ -> return ()
  liftIO $ BS.writeFile (imgDir ++ "/orig" ++ Text.unpack ext) d
  _ <-
    Mongo.insert
      "images"
      ["_id" =: Just objId, "origFile" =: fname, "createdAt" =: current]
  return ()
save _ = return ()

-- * Quoted from https://gist.github.com/eflister/5456125
resize4
  :: (RealFrac a)
  => a -> J.Image PixelRGBA8 -> J.Image PixelRGBA8
resize4 fact i = uncurry (generateImage f) new
  where
    f =
      curry $
      pixelAt4' old (round $ 1 / fact) i .
      uncurry (***) (join (***) tmp (fst, snd))
    old = (imageWidth &&& imageHeight) i
    new = (join (***) $ scale fact) old
    scale r = round . (* toRational r) . toRational
    tmp s = scale (s old) . (% s new)

pixelAt4' :: (Int, Int) -> Int -> J.Image PixelRGBA8 -> (Int, Int) -> PixelRGBA8
pixelAt4' (dw, dh) s i (x, y) = foldl pp (0, 0, 0, 0) pix `pd` length pix
  where
    inds n d = [a | a <- (+ n) <$> [0 .. s], and ([(>= 0), (< d)] <*> [a])]
    pix = uncurry (pixelAt i) <$> [(x', y') | x' <- inds x dw, y' <- inds y dh]
    pp (r, g, b, a) (PixelRGBA8 r' g' b' a') =
      (pf r r', pf g g', pf b b', pf a a')
    pf a = (+ a) . fromIntegral
    pd (r, g, b, a) d = PixelRGBA8 (pr r d) (pr g d) (pr b d) (pr a d)
    pr a b = round (a % b)

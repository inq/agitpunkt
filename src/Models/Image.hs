{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Models.Image where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Time.Clock as TC
import qualified Data.Bson as Bson
import System.Directory (createDirectoryIfMissing)
import Core.Request.Content (Context(..))
import Data.Bson (genObjectId)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Ratio ((%))
import Control.Arrow ((&&&), (***))
import Control.Monad (join)

import Codec.Picture
  ( decodeImage, saveJpgImage, PixelRGB8(PixelRGB8), Image
  , DynamicImage(..)
  , convertRGB8
  , generateImage, imageWidth, imageHeight, pixelAt
  )

data Image = Image
  { _id       :: Maybe Bson.ObjectId
  , filename  :: BS.ByteString
  , content   :: BS.ByteString
  , createdAt :: TC.UTCTime
  }

getDirectory :: TC.UTCTime -> String
getDirectory = formatTime defaultTimeLocale "%y%m%d"

save :: Context -> IO ()
save (MkFile _ _ d) = do
  current <- TC.getCurrentTime
  objId <- genObjectId

  let parentDir = "data/" ++ getDirectory current
      imgDir = parentDir ++ "/" ++ show objId
  createDirectoryIfMissing False parentDir
  createDirectoryIfMissing False imgDir

  case decodeImage d of
    Right i -> do
      let img = convertRGB8 i
      let fact = 1024 % imageWidth img
      if fact > 1
        then saveJpgImage 100 (imgDir ++ "/1024.jpg") $ ImageRGB8 img
        else saveJpgImage 100 (imgDir ++ "/1024.jpg") $ ImageRGB8 $ resize fact img
    _ -> return ()
  BS.writeFile (imgDir ++ "/orig.jpg") d
save _ = return ()


-- * Quoted from https://gist.github.com/eflister/5456125

resize :: (RealFrac a) =>
  a -> Codec.Picture.Image PixelRGB8 -> Codec.Picture.Image PixelRGB8
resize fact i = uncurry (generateImage f) new
    where f = curry $ (pixelAt' old (round $ 1/fact) i) . (uncurry (***) $ (join (***) tmp) (fst,snd))
          old = (imageWidth &&& imageHeight) i
          new = (join (***) $ scale fact) old
          scale r = round . (* (toRational r)) . toRational
          tmp s = scale (s old) . (% (s new))

pixelAt' :: (Int, Int) -> Int -> Codec.Picture.Image PixelRGB8 -> (Int, Int) -> PixelRGB8
pixelAt' (dw,dh) s i (x,y) = avg pix
    where inds n d = [a | a <- (+ n) <$> [0..s], all id ([(>= 0), (< d)] <*> [a])]
          pix = (uncurry $ pixelAt i) <$> [(x',y') | x' <- inds x dw, y' <- inds y dh]
          avg p = (foldl pp (0,0,0) p) `pd` (length p)
          pp (r, g, b) (PixelRGB8 r' g' b') = (pf r r', pf g g', pf b b')
          pf a = (+ a) . fromIntegral
          pd (r, g, b) d = PixelRGB8 (pr r d) (pr g d) (pr b d)
          pr a b = round (a % b)

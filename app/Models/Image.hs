{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Image where

import           Codec.Picture        (DynamicImage (..), PixelRGB8 (PixelRGB8),
                                       convertRGB8, decodeImage, generateImage,
                                       imageHeight, imageWidth, pixelAt,
                                       saveJpgImage)
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
import           System.Directory     (createDirectoryIfMissing)

data Image = Image
  { _id       :: Maybe Bson.ObjectId
  , origFile  :: Text
  , createdAt :: TC.UTCTime
  } deriving (Show)

getDirectory :: TC.UTCTime -> String
getDirectory = formatTime defaultTimeLocale "%y%m%d"

imgUrl :: Image -> Text
imgUrl (Image (Just id') _ created') =
  Text.concat
    [ "/static/data/"
    , pack $ getDirectory created'
    , "/"
    , pack $ show id'
    , "/1024.jpg"
    ]
imgUrl _ = "Unreachable"

find :: Mongo.Action IO [Image]
-- ^ Find images
find = do
  res <- Mongo.find (Mongo.select [] "images") >>= Mongo.rest
  return $ map fromDocument res
  where
    fromDocument doc =
      Image
      { _id = doc !? "_id"
      , origFile = Bson.at "origFile" doc
      , createdAt = Bson.at "createdAt" doc
      }

save :: Context -> Mongo.Action IO ()
-- ^ TODO: Use Bytestring
save (MkFile (Just fname) _ d) = do
  current <- liftIO TC.getCurrentTime
  objId <- liftIO genObjectId
  let parentDir = "data/" ++ getDirectory current
      imgDir = parentDir ++ "/" ++ show objId
  liftIO $ createDirectoryIfMissing False parentDir
  liftIO $ createDirectoryIfMissing False imgDir
  case decodeImage d of
    Right i -> do
      let img = convertRGB8 i
      let fact = 1024 % imageWidth img
      liftIO $
        saveJpgImage 100 (imgDir ++ "/1024.jpg") $
        if fact > 1
          then ImageRGB8 img
          else ImageRGB8 $ resize fact img
    _ -> return ()
  liftIO $ BS.writeFile (imgDir ++ "/orig.jpg") d
  _ <-
    Mongo.insert
      "images"
      ["_id" =: Just objId, "origFile" =: fname, "createdAt" =: current]
  return ()
save _ = return ()

-- * Quoted from https://gist.github.com/eflister/5456125
resize
  :: (RealFrac a)
  => a -> J.Image PixelRGB8 -> J.Image PixelRGB8
resize fact i = uncurry (generateImage f) new
  where
    f =
      curry $
      pixelAt' old (round $ 1 / fact) i .
      uncurry (***) (join (***) tmp (fst, snd))
    old = (imageWidth &&& imageHeight) i
    new = (join (***) $ scale fact) old
    scale r = round . (* toRational r) . toRational
    tmp s = scale (s old) . (% s new)

pixelAt' :: (Int, Int) -> Int -> J.Image PixelRGB8 -> (Int, Int) -> PixelRGB8
pixelAt' (dw, dh) s i (x, y) = avg pix
  where
    inds n d = [a | a <- (+ n) <$> [0 .. s], and ([(>= 0), (< d)] <*> [a])]
    pix = uncurry (pixelAt i) <$> [(x', y') | x' <- inds x dw, y' <- inds y dh]
    avg p = foldl pp (0, 0, 0) p `pd` length p
    pp (r, g, b) (PixelRGB8 r' g' b') = (pf r r', pf g g', pf b b')
    pf a = (+ a) . fromIntegral
    pd (r, g, b) d = PixelRGB8 (pr r d) (pr g d) (pr b d)
    pr a b = round (a % b)

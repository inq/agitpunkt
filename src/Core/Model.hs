{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Core.Model where

import           Core.Database    ()
import qualified Data.Char        as C
import qualified Data.Map.Strict  as M
import           Data.Text        (Text, pack)
import qualified Data.Text        as Text
import qualified Data.Typeable    as T
import           Database.MongoDB ((=:))
import qualified Database.MongoDB as Mongo
import           GHC.Generics     ((:*:) (..))
import qualified GHC.Generics     as GN
import qualified Misc.Json        as Json

data ModelInfo a = ModelInfo
  { table  :: {-# UNPACK #-}!Text
  , fields :: ![(Text, T.TypeRep)]
  } deriving (Show)

class GModel f where
  gFields :: f a -> [(Text, T.TypeRep)]
  gToJson :: f a -> Json.Json
  gToDocument :: f a -> Mongo.Document

-- ^ Generic model
instance GModel GN.U1 where
  gFields _ = []
  gToJson _ = Json.JSArray []
  gToDocument _ = []

-- ^ Constructor without fields
instance (GModel a, GModel b) =>
         GModel (a :*: b) where
  gFields ~(a :*: b) = gFields a ++ gFields b
  gToJson ~(a :*: b) = Json.JSObject $ M.union m1 m2
    where
      Json.JSObject m1 = gToJson a
      Json.JSObject m2 = gToJson b
  gToDocument ~(a :*: b) = gToDocument a ++ gToDocument b

-- ^ Products
instance (GN.Selector c, T.Typeable t, Mongo.Val t, Json.ToJson t) =>
         GModel (GN.M1 GN.S c (GN.K1 GN.R t)) where
  gFields s = [(Text.pack $ GN.selName s, T.typeOf (undefined :: t))]
  gToJson s =
    Json.JSObject $
    case gToJson $ GN.unM1 s of
      Json.JSNil -> M.empty
      val        -> M.fromList [(Text.pack $ GN.selName s, val)]
  gToDocument s =
    case Mongo.val (GN.unK1 $ GN.unM1 s) of
      Mongo.Null -> []
      val        -> [pack (GN.selName s) =: val]

-- ^ Record selector
instance (Json.ToJson a) =>
         GModel (GN.K1 GN.R a) where
  gFields _ = undefined
  gToJson s = Json.toJson $ GN.unK1 s
  gToDocument _ = undefined

-- ^ Parameter (Par ==> Rec)
instance (GModel f) =>
         GModel (GN.M1 GN.C c f) where
  gFields = gFields . GN.unM1
  gToJson = gToJson . GN.unM1
  gToDocument = gToDocument . GN.unM1

-- ^ Constructor
instance (GModel f) =>
         GModel (GN.M1 GN.D c f) where
  gFields = gFields . GN.unM1
  gToJson = gToJson . GN.unM1
  gToDocument = gToDocument . GN.unM1

-- ^ Datatype
class CollectionName f where
  collectionName :: f p -> String

instance (GN.Datatype c) =>
         CollectionName (GN.D1 c f) where
  collectionName = GN.datatypeName

class (GN.Generic a, CollectionName (GN.Rep a), GModel (GN.Rep a)) =>
      Model a where
  modelInfo :: ModelInfo a
  modelInfo =
    ModelInfo
    { table =
        Text.pack $ map C.toLower $ collectionName $ GN.from (undefined :: a)
    , fields = gFields $ GN.from (undefined :: a)
    }
  toDocument :: a -> Mongo.Document
  toDocument = gToDocument . GN.from

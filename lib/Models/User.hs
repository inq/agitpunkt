{-# LANGUAGE OverloadedStrings #-}
module Models.User where

import qualified Manicure.Json                  as Json
import qualified Data.ByteString.Char8          as BS
import qualified Data.Time.Clock                as TC
import qualified Database.MongoDB               as Mongo
import qualified Database.MongoDB.Query         as MQ
import qualified Database.Redis                 as R
import qualified Data.Bson                      as Bson
import qualified Data.Map                       as M
import Control.Monad (liftM)
import Data.Map ((!))

import Database.MongoDB ((=:))

data User = User {
  _id         :: Maybe Bson.ObjectId,
  facebook_id :: BS.ByteString,
  email       :: BS.ByteString,
  name        :: BS.ByteString,
  created_at  :: Maybe TC.UTCTime
} deriving Show

from_json :: BS.ByteString -> User
-- ^ Construct an User data from the JSON Bytestring
from_json bs = User Nothing facebook_id email name Nothing
  where
    Json.JSObject json        = Json.parse bs
    Json.JSString facebook_id = json ! "id"
    Json.JSString email       = json ! "email"
    Json.JSString name        = json ! "name"

upsert :: User -> Mongo.Action IO ()
-- ^ Update or insert the User
upsert (User _ facebook_id email name created_at) = do
    Mongo.upsert (MQ.Select ["facebook_id" =: Bson.Binary facebook_id] "users") [
        "facebook_id" =: Bson.Binary facebook_id,
        "email"       =: Bson.Binary email,
        "name"        =: Bson.Binary name,
        "created_at"  =: created_at
      ]
    return ()

redis_hash :: BS.ByteString -> User -> R.Redis (Either R.Reply R.Status)
-- ^ Save the data into the Redis
redis_hash key (User _ facebook_id email name created_at) = R.hmset key values
  where
    values = [
        ("facebook_id", facebook_id),
        ("email",       email),
        ("name",        name),
        ("created_at",  BS.pack $ show created_at)
      ]

from_map :: M.Map BS.ByteString BS.ByteString -> User
-- ^ Construct an user from the given map
from_map map = 
    User Nothing (map ! "facebook_id") (map ! "email") (map ! "name") Nothing

redis_get :: BS.ByteString -> R.Redis (Maybe User)
-- ^ Find a user by session key
redis_get key = do
    res <- R.hgetall key
    return $ case res of 
        Left  _    -> Nothing
        Right list -> Just $ from_map (M.fromList list)

save :: User -> Mongo.Action IO ()
-- ^ Save the data into the DB
save (User _id facebook_id email name (Just created_at)) = do
    Mongo.insert "users" [
        "facebook_id" =: Bson.Binary facebook_id,
        "email"       =: Bson.Binary email,
        "name"        =: Bson.Binary name,
        "created_at"  =: created_at
      ]
    return ()

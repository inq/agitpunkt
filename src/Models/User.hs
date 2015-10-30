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
  id         :: Maybe Bson.ObjectId,
  facebookId :: BS.ByteString,
  name       :: BS.ByteString,
  createdAt  :: Maybe TC.UTCTime
} deriving Show

fromJson :: BS.ByteString -> User
-- ^ Construct an User data from the JSON Bytestring
fromJson bs = User Nothing facebookId name Nothing
  where
    Json.JSObject json       = Json.parse bs
    Json.JSString facebookId = json ! "id"
    Json.JSString name       = json ! "name"

upsert :: User -> Mongo.Action IO ()
-- ^ Update or insert the User
upsert (User _ facebookId name createdAt) = do
    Mongo.upsert (MQ.Select ["facebook_id" =: Bson.Binary facebookId] "users") [
        "facebook_id" =: Bson.Binary facebookId,
        "name"        =: Bson.Binary name,
        "created_at"  =: createdAt
      ]
    return ()

redisHash :: BS.ByteString -> User -> R.Redis (Either R.Reply R.Status)
-- ^ Save the data into the Redis
redisHash key (User _ facebookId name createdAt) = R.hmset key values
  where
    values = [
        ("facebook_id", facebookId),
        ("name",        name),
        ("created_at",  BS.pack $ show createdAt)
      ]

fromMap :: M.Map BS.ByteString BS.ByteString -> User
-- ^ Construct an user from the given map
fromMap map = 
    User Nothing (map ! "facebook_id") (map ! "name") Nothing

redisGet :: BS.ByteString -> R.Redis (Maybe User)
-- ^ Find a user by session key
redisGet key = do
    res <- R.hgetall key
    return $ case res of 
        Left  _    -> Nothing
        Right list -> Just $ fromMap (M.fromList list)

save :: User -> Mongo.Action IO ()
-- ^ Save the data into the DB
save (User id facebookId name (Just createdAt)) = do
    Mongo.insert "users" [
        "facebook_id" =: Bson.Binary facebookId,
        "name"        =: Bson.Binary name,
        "created_at"  =: createdAt
      ]
    return ()

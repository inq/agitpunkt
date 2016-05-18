{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Models.User where

import qualified Core.Json                        as Json
import qualified Data.ByteString.Char8            as BS
import qualified Data.Time.Clock                  as TC
import qualified Database.MongoDB                 as Mongo
import qualified Database.MongoDB.Query           as MQ
import qualified Database.Redis                   as R
import qualified Data.Bson                        as Bson
import qualified Data.Map                         as M
import qualified Core.Model                       as Model
import qualified Crypto.Hash.SHA256               as SHA256
import qualified Data.ByteString.Unsafe           as BSU
import qualified Data.ByteString.Internal         as BSI
import qualified Data.Bits                        as B
import qualified Foreign.Storable                 as FS
import qualified Foreign.Ptr                      as FP
import qualified Data.ByteString.UTF8             as UTF8
import GHC.Generics (Generic)
import Control.Monad (liftM)
import Data.Bson ((!?))
import Data.Map ((!))
import Data.Bits ((.&.))

import Database.MongoDB ((=:))

data User = User 
  { _id        :: Maybe Bson.ObjectId
  , email      :: BS.ByteString
  , name       :: BS.ByteString
  , password   :: Maybe BS.ByteString
  , createdAt :: Maybe TC.UTCTime
  } deriving (Show, Generic)

instance Model.Model User

hashPassword :: BS.ByteString -> BS.ByteString
-- ^ Hash the given password
hashPassword = toHex . SHA256.hash
  where
    toHex bs = BSI.unsafeCreate nl $ go 0
      where
        len = BS.length bs
        nl = 2 * len
        go i p
          | i == len  = return ()
          | otherwise = case BSU.unsafeIndex bs i of
              w -> do 
                  FS.poke p (hexDigest $ w `B.shiftR` 4)
                  FS.poke (p `FP.plusPtr` 1) (hexDigest $ w .&. 0xF)
                  go (i + 1) (p `FP.plusPtr` 2)
    hexDigest d
        | d < 10 = d + 48
        | otherwise = d + 87

signIn :: BS.ByteString -> BS.ByteString -> Mongo.Action IO (Maybe User)
-- ^ Try to signin
signIn email password = do
    res <- Mongo.find (Mongo.select ["email" =: email, "password" =: password] "users") >>= MQ.rest
    case res of 
        [doc] -> do
             return $ Just $ User
                   { _id = (doc !? "_id")
                   , email = (Bson.at "email" doc)
                   , name = (Bson.at "name" doc)
                   , password = Nothing
                   , createdAt = Nothing
                   }
        _     -> do
             return Nothing

upsert :: User -> Mongo.Action IO ()
-- ^ Update or insert the User
upsert user@User{_id=_id} = do
    Mongo.upsert (MQ.Select ["_id" =: _id] "users") $ Model.toDocument user
    return ()

redisHash :: BS.ByteString -> User -> R.Redis (Either R.Reply R.Status)
-- ^ Save the data into the Redis
redisHash key (User _ email name password createdAt) = R.hmset key values
  where
    values = 
      [ ("name" :: BSI.ByteString, name)
      , ("email" :: BSI.ByteString, email)
      ]

fromMap :: M.Map BS.ByteString BS.ByteString -> User
-- ^ Construct an user from the given map
fromMap map = 
    User 
      { _id = Nothing
      , email = (map ! "email")
      , name = (map ! "name")
      , password = Nothing
      , createdAt = Nothing
      }

redisGet :: BS.ByteString -> R.Redis (Maybe User)
-- ^ Find a user by session key
redisGet key = do
    res <- R.hgetall key
    return $ case res of 
        Left  _    -> Nothing
        Right []   -> Nothing
        Right list -> Just $ fromMap (M.fromList list)

save :: User -> Mongo.Action IO ()
-- ^ Save the data into the DB
save user = do
    Mongo.insert "users" $ Model.toDocument user
    return ()

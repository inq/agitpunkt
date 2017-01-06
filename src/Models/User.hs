{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Models.User where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Time.Clock as TC
import qualified Database.MongoDB as Mongo
import qualified Database.MongoDB.Query as MQ
import qualified Data.Bson as Bson
import qualified Data.Map as M
import qualified Core.Model as Model
import qualified Data.ByteString.Internal as BSI
import GHC.Generics (Generic)
import Data.Bson ((!?))
import Data.Map ((!))

import Database.MongoDB ((=:))

data User = User
  { _id       :: Maybe Bson.ObjectId
  , email     :: BS.ByteString
  , name      :: BS.ByteString
  , password  :: Maybe BS.ByteString
  , createdAt :: Maybe TC.UTCTime
  } deriving (Show, Generic)

instance Model.Model User

signIn :: BS.ByteString -> BS.ByteString -> Mongo.Action IO (Maybe User)
-- ^ Try to signin
signIn email' password' = do
    res <- Mongo.find (Mongo.select ["email" =: email', "password" =: password'] "users")
        >>= MQ.rest
    case res of
        [doc] -> return $ Just User
            { _id = doc !? "_id"
            , email = Bson.at "email" doc
            , name = Bson.at "name" doc
            , password = Nothing
            , createdAt = Nothing
            }
        _     -> return Nothing

upsert :: User -> Mongo.Action IO ()
-- ^ Update or insert the User
upsert user@User{_id=_id} = do
    Mongo.upsert (MQ.Select ["_id" =: _id] "users") $ Model.toDocument user
    return ()

fromMap :: M.Map BS.ByteString BS.ByteString -> User
-- ^ Construct an user from the given map
fromMap map' =
    User
      { _id = Nothing
      , email = map' ! "email"
      , name = map' ! "name"
      , password = Nothing
      , createdAt = Nothing
      }

save :: User -> Mongo.Action IO ()
-- ^ Save the data into the DB
save user = do
    _ <- Mongo.insert "users" $ Model.toDocument user
    return ()

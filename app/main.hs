{-# LANGUAGE OverloadedStrings #-}
import qualified Network                        as N
import qualified Network.Socket                 as NS
import qualified Network.Socket.ByteString      as NSB 
import qualified Control.Concurrent             as CC
import qualified Manicure.Route                 as Route
import qualified Manicure.Request               as Req
import qualified Manicure.Response              as Res
import qualified Manicure.Database              as DB
import qualified Manicure.Handler               as Handler
import qualified Manicure.Session               as Session
import qualified Manicure.Json                  as Json
 
import qualified Crypto.Hash.SHA256             as SHA256
import qualified Data.ByteString.Char8          as BS

main :: IO ()
-- ^ The main function
main = N.withSocketsDo $ do
    key <- Session.generateKey
    putStrLn (BS.unpack key)
    db <- DB.connect "test"
    socketFd <- NS.socket NS.AF_UNIX NS.Stream 0
    NS.bind socketFd $ NS.SockAddrUnix "manicure.sock"
    NS.listen socketFd 10
    acceptSocket socketFd db

acceptSocket :: NS.Socket -> DB.Connection -> IO ()
-- ^ Accept a new socket with a new process
acceptSocket socketFd db = do
    (fd, _) <- NS.accept socketFd
    CC.forkIO $ acceptBody fd db
    acceptSocket socketFd db

acceptBody :: NS.Socket -> DB.Connection -> IO () 
-- ^ Process the connection
acceptBody fd db = do
    req <- NSB.recv fd 4096
    let request = Req.parse req fd
    let uri = Req.uri request
    let method = Req.method request
    response <- (Route.match uri method Handler.routeTree) db request
    NSB.sendAll fd $ Res.render response
    NS.sClose fd

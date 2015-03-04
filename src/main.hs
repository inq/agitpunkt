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
 
main :: IO ()
-- ^ The main function
main = N.withSocketsDo $ do
    db <- DB.connect "test"
    socket_fd <- NS.socket NS.AF_UNIX NS.Stream 0
    NS.bind socket_fd $ NS.SockAddrUnix "manicure.sock"
    NS.listen socket_fd 10
    accept_socket socket_fd db

accept_socket :: NS.Socket -> DB.Connection -> IO ()
-- ^ Accept a new socket with a new process
accept_socket socket_fd db = do
    (fd, _) <- NS.accept socket_fd
    CC.forkIO $ accept_body fd db
    accept_socket socket_fd db

accept_body :: NS.Socket -> DB.Connection -> IO () 
-- ^ Process the connection
accept_body fd db = do
    _request <- NSB.recv fd 4096
    let request = Req.parse _request fd
    let uri = Req.uri request
    let method = Req.method request
    response <- (Route.match uri method Handler.route_tree) db request
    NSB.sendAll fd $ Res.render response
    NS.sClose fd

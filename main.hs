{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
import qualified Data.ByteString.Char8          as BS
import qualified Language.Haskell.TH.Quote      as TQ
import qualified Language.Haskell.TH.Syntax     as TS
import qualified Network                        as N
import qualified Network.Socket                 as NS
import qualified Network.Socket.ByteString      as NSB 
import qualified Control.Concurrent             as CC

import qualified Manicure.Route                 as Route
import qualified Manicure.Request               as Req
import qualified Manicure.Response              as Res
import Handler.Main
 
main = N.withSocketsDo $ do
    socket_fd <- NS.socket NS.AF_UNIX NS.Stream 0
    NS.bind socket_fd $ NS.SockAddrUnix "manicure.sock"
    NS.listen socket_fd 10
    accept_socket socket_fd

accept_socket :: NS.Socket -> IO ()
accept_socket socket_fd = do
    (fd, _) <- NS.accept socket_fd
    CC.forkIO $ accept_body fd
    accept_socket socket_fd

accept_body :: NS.Socket -> IO () 
accept_body fd = do
    request <- NSB.recv fd 4096
    NSB.sendAll fd $ Res.render $ Route.extract routes $ Req.parse request fd
    NS.sClose fd

parse_request :: BS.ByteString -> [BS.ByteString] 
parse_request request =
    BS.split '\n' request

routes = $(Route.parseFile "config/routes.cfg")

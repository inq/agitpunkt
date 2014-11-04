{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
import qualified Data.ByteString.Char8 as BS
import qualified Manicure.Route as Route
import qualified Manicure.Request as Request
 
import Network (PortID(PortNumber), withSocketsDo, listenOn)
import Network.Socket (SocketType(Stream), Family(AF_UNIX),
    SockAddr(SockAddrUnix), Socket, 
    accept, bind, listen, sClose, socket)
import Network.Socket.ByteString (sendAll, recv)
import Control.Concurrent

 
main = withSocketsDo $ do
    socket_fd <- socket AF_UNIX Stream 0
    bind socket_fd $ SockAddrUnix ("manicure.sock")
    listen socket_fd 10
    accept_socket socket_fd

accept_socket :: Socket -> IO ()
accept_socket socket_fd = do
    (fd, _) <- accept socket_fd
    forkIO $ accept_body fd
    accept_socket socket_fd

accept_body :: Socket -> IO () 
accept_body fd = do
    request <- recv fd 4096
    sendAll fd $ response $ show $ sample
    sClose fd

parse_request :: BS.ByteString -> [BS.ByteString] 
parse_request request =
    BS.split '\n' request

response :: String -> BS.ByteString
response str =
    BS.pack $ "HTTP/1.0 200 OK\r\nContent-Length: " ++ (show $ length str) ++ "\r\n\r\n" ++ str ++ "\r\n"

sample :: Route.Routes
sample = [Route.parse|
/ GET index


/ POST post_test

|]

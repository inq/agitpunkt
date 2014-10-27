import Data.ByteString.Char8 as BS
 
import Network (PortID(PortNumber), withSocketsDo, listenOn)
import Network.Socket (accept, bind, listen, sClose, socket, SocketType(Stream), Family(AF_UNIX), SockAddr(SockAddrUnix))
import Network.Socket.ByteString (sendAll, recv)
import Control.Concurrent
 
main = withSocketsDo $ do
    socket_fd <- socket AF_UNIX Stream 0
    bind socket_fd $ SockAddrUnix ("manicure.sock")
    listen socket_fd 10
    accept_socket socket_fd
 
accept_socket socket_fd = do
    (fd, _) <- accept socket_fd
    forkIO $ accept_body fd
    accept_socket socket_fd
  
accept_body c = do
    request <- recv c 4096
    BS.putStr $ Prelude.head $ parse_request request
    sendAll c msg
    sClose c
 
parse_request request =
    BS.split '\n' $ request

msg = BS.pack "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

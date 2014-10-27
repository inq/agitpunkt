import Data.ByteString.Char8 as BS
 
import Network (PortID(PortNumber), withSocketsDo, listenOn)
import Network.Socket (accept, sClose)
import Network.Socket.ByteString (sendAll, recv)
import Control.Concurrent
 
main = withSocketsDo $ do
    socket_fd <- listenOn $ PortNumber 5000
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

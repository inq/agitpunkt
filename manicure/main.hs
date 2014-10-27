import Data.ByteString.Char8 as BS
 
import Network (PortID(PortNumber), withSocketsDo, listenOn)
import Network.Socket (accept, sClose)
import Network.Socket.ByteString (sendAll, recv)
import Control.Concurrent
 
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 5002
    loop sock
 
loop sock = do
    (conn, _) <- accept sock
    forkIO $ body conn
    loop sock
  where
    body c = do
        hello <- recv c 4096
        BS.putStr hello
        sendAll c msg
        sClose c
 
msg = BS.pack "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

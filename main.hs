{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
import qualified Data.ByteString.Char8 as BS
import qualified Language.Haskell.TH.Quote  as TQ
import qualified Language.Haskell.TH.Syntax as TS
import qualified Network as N
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB 
import qualified Control.Concurrent as CC

import qualified Manicure.Route as Route
import qualified Manicure.Request as Request
 
main = N.withSocketsDo $ do
    putStrLn $ $(return $ TS.VarE $ TS.mkName "index")
    socket_fd <- NS.socket NS.AF_UNIX NS.Stream 0
    NS.bind socket_fd $ NS.SockAddrUnix ("manicure.sock")
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
    putStrLn $ show $ Request.parse request fd    
    NSB.sendAll fd $ response $ show $ index
    NS.sClose fd

parse_request :: BS.ByteString -> [BS.ByteString] 
parse_request request =
    BS.split '\n' request

response :: String -> BS.ByteString
response str =
    BS.concat [
      "HTTP/1.0 200 OK\r\nContent-Length: ", 
      BS.pack $ show $ length str,
      "\r\n\r\n", 
      BS.pack str, 
      "\r\n"
    ]

index :: String
index = "HELLO"

routes = $(Route.parseFile "config/routes.cfg")

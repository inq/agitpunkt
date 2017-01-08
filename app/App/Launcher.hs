{-# LANGUAGE OverloadedStrings #-}
module App.Launcher where

import qualified Network.Socket.ByteString.Lazy as LazySocket
import qualified Network.Socket                 as NS
import qualified Data.ByteString.Char8          as BS
import qualified Core.Database                  as DB
import qualified Core.Request as Req
import qualified Core.Response as Res
import Network.Socket
  ( Family(AF_UNIX), SocketType(Stream), SockAddr(SockAddrUnix), Socket
  , socket, bind, listen
  )
import Network.Socket.ByteString (sendAll)
import Control.Concurrent (forkIO)
import Misc.File (removeIfExists, setStdFileMode)
import Network (withSocketsDo)
import App.Component (ResState(..), runHandler)
import App.Route (RouteTree, match)
import App.Session (SessionStore, initStore)

run :: RouteTree -> BS.ByteString -> BS.ByteString -> String -> IO ()
-- ^ Run the given RouteTree
run rt response404 databaseName socketFile = withSocketsDo $ do
    _ <- removeIfExists socketFile -- TODO: handle exceptions
    db <- DB.connect databaseName
    ss <- initStore
    socketFd <- socket AF_UNIX Stream 0
    bind socketFd $ SockAddrUnix socketFile
    listen socketFd 10
    setStdFileMode socketFile
    acceptSocket rt response404 socketFd db ss

acceptSocket :: RouteTree -> BS.ByteString -> Socket -> DB.Connection -> SessionStore -> IO ()
-- ^ Accept a new socket with a new process
acceptSocket rt response404 socketFd db ss = do
    (fd, _) <- NS.accept socketFd
    _ <- forkIO $ acceptBody rt response404 fd db ss
    acceptSocket rt response404 socketFd db ss

acceptBody :: RouteTree -> BS.ByteString -> Socket -> DB.Connection -> SessionStore -> IO ()
-- ^ Process the connection
acceptBody rt response404 fd db ss = do
    req' <- LazySocket.getContents fd
    let request = Req.parse req' fd
    let uri = Req.uri request
    let method = Req.method request
    (response, _state) <- case match uri method rt of
        Just (handler, paramRes) -> do
            putStrLn "handler"
            runHandler handler paramRes db request ss
        Nothing -> do
            putStrLn "nothing"
            return (Res.error 404 response404, ResState db [] request ss)
    print response
    sendAll fd $ Res.render response
    NS.close fd

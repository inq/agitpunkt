{-# LANGUAGE OverloadedStrings #-}

module App.Launcher where

import           App.Component                  (ResState (..), runHandler)
import           App.Route                      (RouteTree, match)
import           App.Session                    (SessionStore, initStore)
import           Control.Concurrent             (forkIO)
import qualified Core.Database                  as DB
import qualified Core.Request                   as Req
import qualified Core.Response                  as Res
import           Data.Text                      (Text)
import           Data.Text.Encoding             (encodeUtf8)
import           Data.Text.Lazy.Encoding        (decodeUtf8)
import           Misc.File                      (removeSockIfExists,
                                                 setStdFileMode)
import           Models.User                    (UserStore, loadUserStore,
                                                 putUserStore)
import           Network                        (withSocketsDo)
import           Network.Socket                 (Family (AF_UNIX),
                                                 SockAddr (SockAddrUnix),
                                                 Socket, SocketType (Stream),
                                                 bind, listen, socket)
import qualified Network.Socket                 as NS
import           Network.Socket.ByteString      (sendAll)
import qualified Network.Socket.ByteString.Lazy as LazySocket

run :: RouteTree -> Text -> Text -> String -> IO ()
-- ^ Run the given RouteTree
run rt response404 databaseName socketFile =
  withSocketsDo $ do
    _ <- removeSockIfExists socketFile -- TODO: handle exceptions
    db <- DB.connect databaseName
    ss <- initStore
    Just us <- loadUserStore "config/users.tsv"
    putUserStore us
    socketFd <- socket AF_UNIX Stream 0
    bind socketFd $ SockAddrUnix socketFile
    listen socketFd 10
    setStdFileMode socketFile
    acceptSocket rt response404 socketFd db ss us

acceptSocket
  :: RouteTree
  -> Text
  -> Socket
  -> DB.Connection
  -> SessionStore
  -> UserStore
  -> IO ()
-- ^ Accept a new socket with a new process
acceptSocket rt response404 socketFd db ss us = do
  (fd, _) <- NS.accept socketFd
  _ <- forkIO $ acceptBody rt response404 fd db ss us
  acceptSocket rt response404 socketFd db ss us

acceptBody
  :: RouteTree
  -> Text
  -> Socket
  -> DB.Connection
  -> SessionStore
  -> UserStore
  -> IO ()
-- ^ Process the connection
--   TODO: Use ByteString
acceptBody rt response404 fd db ss us = do
  req' <- decodeUtf8 <$> LazySocket.getContents fd
  let request = Req.parse req' fd
  let uri = Req.uri request
  let method = Req.method request
  (response, _state) <-
    case match uri method rt of
      Just (handler, paramRes) -> do
        putStrLn "handler"
        runHandler handler paramRes db request ss us
      Nothing -> do
        putStrLn "nothing"
        return (Res.error 404 response404, ResState db [] request ss us)
  print response
  sendAll fd $ encodeUtf8 (Res.render response)
  NS.close fd

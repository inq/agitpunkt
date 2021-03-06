{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Launcher where

import           App.Component                  (ResState (..), runHandler)
import           App.Route                      (RouteTree, match)
import           App.Session                    (SessionStore, initStore)
import           Control.Concurrent             (forkIO)
import qualified Core.Database                  as DB
import qualified Core.Request                   as Request
import qualified Core.Response                  as Res
import           Data.Text                      (Text)
import           Misc.File                      (removeSockIfExists,
                                                 setStdFileMode)
import           Models.User                    (UserStore, loadUserStore,
                                                 putUserStore)
import           Network.Socket                 (Family (AF_UNIX),
                                                 SockAddr (SockAddrUnix),
                                                 Socket, SocketType (Stream),
                                                 bind, listen, socket, withSocketsDo)
import qualified Network.Socket                 as NS
import           Network.Socket.ByteString      (sendAll)
import qualified Network.Socket.ByteString.Lazy as LazySocket

run :: RouteTree -> Text -> Text -> String -> IO ()
-- ^ Run the given RouteTree
run rt response404 databaseName socketFile =
  withSocketsDo $ do
    removeSockIfExists socketFile
    db <- DB.connect databaseName
    ss <- initStore
    loadUserStore "config/users.tsv" >>= \case
      Just user_store -> do
        putUserStore user_store
        socketFd <- socket AF_UNIX Stream 0
        bind socketFd $ SockAddrUnix socketFile
        listen socketFd 10
        setStdFileMode socketFile
        acceptSocket rt response404 socketFd db ss user_store
      Nothing ->
        putStrLn "FATAL: Cannot load user store."

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
  _thread_id <- forkIO $ acceptBody rt response404 fd db ss us
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
acceptBody rt response404 fd db ss us = do
  buffer <- LazySocket.getContents fd
  let request = Request.parse buffer fd
  let uri = Request.uri request
  let method = Request.method request
  (response, _state) <-
    case match uri method rt of
      Just (handler, paramRes) -> runHandler handler paramRes db request ss us
      Nothing ->
        return (Res.error 404 response404, ResState db [] request ss us)
  print response
  sendAll fd $ Res.render response
  NS.close fd

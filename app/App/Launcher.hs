{-# LANGUAGE OverloadedStrings #-}
module App.Launcher where

import qualified Network.Socket                 as NS
import qualified Network.Socket.ByteString      as NSB
import qualified Network.Socket.ByteString.Lazy as NSL
import qualified Data.ByteString.Char8          as BS
import qualified Core.Database                  as DB
import qualified Control.Concurrent             as CC
import qualified System.Posix.Process           as P
import qualified System.Posix.Files             as PF
import qualified Core.Request as Req
import qualified Core.Response as Res
import System.Posix.Signals (signalProcess, sigQUIT)
-- TODO: wrap these
import System.Posix.IO
  ( OpenMode(ReadOnly, ReadWrite)
  , openFd, closeFd, dupTo
  , fdWrite
  , createFile
  , stdInput, stdOutput, stdError
  , defaultFileFlags,
  )
import System.Directory (doesFileExist, removeFile)
import Control.Monad (when, void)
import Network (withSocketsDo)
import App.Component (ResState(..), runHandler)
import App.Route (RouteTree, match)
import App.Session (SessionStore, initStore)

daemonize :: String -> String -> String -> IO () -> IO ()
-- ^ Daemonize the given function
daemonize pidFile stdOut stdErr process = do
    exists <- doesFileExist pidFile
    when exists $ removeAndKill pidFile
    void $ P.forkProcess $ do
        _ <- P.createSession
        void $ P.forkProcess $ do
            _ <- writePid pidFile
            remapFds
            process
  where
    remapFds = do
        devNull <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
        _ <- dupTo devNull stdInput
        closeFd devNull
        fdOut <- openFd stdOut ReadWrite (Just PF.stdFileMode) defaultFileFlags
        _ <- dupTo fdOut stdOutput
        closeFd fdOut
        fdErr <- openFd stdErr ReadWrite (Just PF.stdFileMode) defaultFileFlags
        _ <- dupTo fdErr stdError
        closeFd fdErr
    writePid _pidFile = do
        fd <- createFile _pidFile PF.stdFileMode
        pid <- P.getProcessID
        print pid
        fdWrite fd (show pid)
    removeAndKill _pidFile = do
        pid <- readFile _pidFile
        removeFile _pidFile
        putStrLn ("pid file exists: " ++ pid)
        signalProcess sigQUIT $ read pid

run :: RouteTree -> BS.ByteString -> BS.ByteString -> String -> IO ()
-- ^ Run the given RouteTree
run rt response404 databaseName socketFile = withSocketsDo $ do
    removeExistingSocket socketFile
    db <- DB.connect databaseName
    ss <- initStore
    socketFd <- NS.socket NS.AF_UNIX NS.Stream 0
    NS.bind socketFd $ NS.SockAddrUnix socketFile
    NS.listen socketFd 10
    PF.setFileMode socketFile PF.stdFileMode
    acceptSocket rt response404 socketFd db ss
  where
    removeExistingSocket _socketFile = do
      exists <- doesFileExist _socketFile
      when exists $ removeFile _socketFile

acceptSocket :: RouteTree -> BS.ByteString -> NS.Socket -> DB.Connection -> SessionStore -> IO ()
-- ^ Accept a new socket with a new process
acceptSocket rt response404 socketFd db ss = do
    (fd, _) <- NS.accept socketFd
    _ <- CC.forkIO $ acceptBody rt response404 fd db ss
    acceptSocket rt response404 socketFd db ss

acceptBody :: RouteTree -> BS.ByteString -> NS.Socket -> DB.Connection -> SessionStore -> IO ()
-- ^ Process the connection
acceptBody rt response404 fd db ss = do
    req' <- NSL.getContents fd
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
    NSB.sendAll fd $ Res.render response
    NS.close fd

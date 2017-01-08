module Misc.Daemon
  ( daemonize
  ) where

import System.Posix.Process (forkProcess,  createSession)
import System.Posix.Signals (signalProcess, sigQUIT)
import Control.Monad (void)
import Misc.File (remapFds, removeIfExists, writePid)

daemonize :: String -> String -> String -> IO () -> IO ()
-- ^ Daemonize the given function
daemonize pidFile outFile errFile process = do
    res <- removeIfExists pidFile
    case res of
      Just pid -> do
        putStrLn ("pid file exists: " ++ pid)
        signalProcess sigQUIT $ read pid
      Nothing -> return ()
    void $ forkProcess $ do
        _ <- createSession
        void $ forkProcess $ do
            writePid pidFile
            remapFds outFile errFile
            process

module Misc.Daemon
  ( daemonize
  ) where

import           Control.Monad        (void)
import           Misc.File            (remapFds, removeIfExists, writePid)
import           System.Posix.Process (createSession, forkProcess)
import           System.Posix.Signals (sigQUIT, signalProcess)

daemonize :: String -> String -> String -> IO () -> IO ()
-- ^ Daemonize the given function
daemonize pidFile outFile errFile process = do
  res <- removeIfExists pidFile
  case res of
    Just pid -> do
      putStrLn ("pid file exists: " ++ pid)
      signalProcess sigQUIT $ read pid
    Nothing -> return ()
  void $
    forkProcess $ do
      _ <- createSession
      void $
        forkProcess $ do
          writePid pidFile
          remapFds outFile errFile
          process

module Misc.File
  ( remapFds
  , setStdFileMode
  , writePid
  , removeIfExists
  ) where
-- ^ Wrapper module for the file operations

import Control.Monad (void)
import System.Posix.Process (getProcessID)
import System.Posix.Files (stdFileMode, setFileMode)
import System.Directory (doesFileExist, removeFile)
import System.Posix.IO
  ( OpenMode(ReadOnly, ReadWrite)
  , openFd, closeFd, dupTo
  , fdWrite
  , createFile
  , stdInput, stdOutput, stdError
  , defaultFileFlags,
  )

setStdFileMode :: FilePath -> IO ()
-- ^ Grant the file permission 666
setStdFileMode theFile = setFileMode theFile stdFileMode

removeIfExists :: FilePath -> IO (Maybe String)
-- ^ Remove the file and return True if the file exists
removeIfExists theFile = do
  exists <- doesFileExist theFile
  if exists
    then do
      content <- readFile theFile
      removeFile theFile
      return $ Just content
    else
      return Nothing

remapFds :: FilePath -> FilePath -> IO ()
-- ^ Remap stdout & stderr to the specified fds
remapFds outFile errFile = do
  devNull <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
  void $ dupTo devNull stdInput
  closeFd devNull
  fdOut <- openFd outFile ReadWrite (Just stdFileMode) defaultFileFlags
  void $ dupTo fdOut stdOutput
  closeFd fdOut
  fdErr <- openFd errFile ReadWrite (Just stdFileMode) defaultFileFlags
  void $ dupTo fdErr stdError
  closeFd fdErr

writePid :: FilePath -> IO ()
-- ^ Write pid file
-- TODO: handle exceptions
writePid _pidFile = do
  fd <- createFile _pidFile stdFileMode
  pid <- getProcessID
  print pid
  void $ fdWrite fd (show pid)

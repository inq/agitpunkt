{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
import qualified Data.ByteString.Char8          as BS
import qualified Core.Handler                   as Handler
import qualified Core.Launcher                  as Launcher
import Core.Html (parse)

main :: IO ()
-- ^ The main function
main = do
    response404 <- [parse|html
      body
        div
          p
            | oops 404!
     |]
    let process = Launcher.run Handler.routeTree (BS.concat response404) databaseName socketFile
    Launcher.daemonize pidFile stdOut stdErr process
  where
    databaseName = "manicure-test"
    socketFile = "tmp/sockets/manicure.sock"
    pidFile = "tmp/pids/manicure.pid"
    stdOut = "tmp/logs/stdout"
    stdErr = "tmp/logs/stderr"

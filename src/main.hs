{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
import qualified Core.Handler                   as Handler
import qualified Core.Launcher                  as Launcher
import Core.Html (parse)

main :: IO ()
-- ^ The main function
main = do
    Launcher.daemonize pidFile stdOut stdErr process
  where
    process = Launcher.run Handler.routeTree response404 databaseName socketFile
    response404 = [parse|html
      body
        div
          p
            | oops 404!
     |]
    databaseName = "manicure-test"
    socketFile = "tmp/sockets/manicure.sock"
    pidFile = "tmp/pids/manicure.pid"
    stdOut = "tmp/logs/stdout"
    stdErr = "tmp/logs/stderr"

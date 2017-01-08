{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
import App.Handler (routeTree)
import App.Launcher (run)
import Misc.Daemon (daemonize)
import Misc.Html (parse)

main :: IO ()
-- ^ The main function
main = do
    response404 <- [parse|html
      body
        div
          p
            | oops 404!
     |]
    let process = run routeTree response404 databaseName socketFile
    daemonize pidFile stdOut stdErr process
  where
    databaseName = "manicure-test"
    socketFile = "tmp/sockets/manicure.sock"
    pidFile = "tmp/pids/manicure.pid"
    stdOut = "tmp/logs/stdout"
    stdErr = "tmp/logs/stderr"

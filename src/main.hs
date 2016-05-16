{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
import qualified Core.Handler                   as Handler
import qualified Core.Launcher                  as Launcher
import qualified Core.Html                      as Html

main :: IO ()
-- ^ The main function
main = do
    Launcher.daemonize pidFile stdOut stdErr process
  where
    process = Launcher.run Handler.routeTree response404 databaseName socketFile
    response404 = $(Html.parseFile "404.html.qh")
    databaseName = "manicure-test"
    socketFile = "tmp/sockets/manicure.sock"
    pidFile = "tmp/pids/manicure.pid"
    stdOut = "tmp/logs/stdout"
    stdErr = "tmp/logs/stderr"


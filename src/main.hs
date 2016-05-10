{-# LANGUAGE OverloadedStrings #-}
import qualified Core.Handler                   as Handler
import qualified Core.Launcher                  as Launcher

main :: IO ()
-- ^ The main function
main = Launcher.daemonize pidFile $ Launcher.run Handler.routeTree databaseName socketFile
  where
    databaseName = "manicure-test"
    socketFile = "tmp/sockets/manicure.sock"
    pidFile = "tmp/pids/manicure.pid"


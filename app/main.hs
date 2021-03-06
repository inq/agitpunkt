{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           App.Handler  (routeTree)
import           App.Launcher (run)
import           Misc.Html    (parse)

main :: IO ()
-- ^ The main function
main = do
  response404 <-
    [parse|html
      body
        div
          p
            | oops 404!
     |]
  run routeTree response404 databaseName socketFile
  where
    databaseName = "agitpunkt"
    socketFile = "tmp/sockets/agitpunkt.sock" --    pidFile = "tmp/pids/agitpunkt.pid"
--    stdOut = "tmp/logs/stdout"
--    stdErr = "tmp/logs/stderr"

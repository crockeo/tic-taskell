module Main where

--------------------
-- Global Imports --
import Web.Scotty

-------------------
-- Local Imports --
import Server
import Board

-- | The entry point to the application. Boots up the threads for the @'Board'@
--   and also for the server.
main :: IO ()
main = do
  scotty 3000 server

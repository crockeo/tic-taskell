module Main where

--------------------
-- Global Imports --
import Data.IORef
import Web.Scotty

-------------------
-- Local Imports --
import Server
import Board

-- | The global config for the port.
port :: Int
port = 3000

-- | The entry point to the application. Boots up the threads for the @'Board'@
--   and also for the server.
main :: IO ()
main = do
  boardRef <- newIORef defaultBoard
  scotty port $ server boardRef

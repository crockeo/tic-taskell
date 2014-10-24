module Server where

--------------------
-- Global Imports --
import Data.IORef
import Web.Scotty

-------------------
-- Local Imports --
import Board

----------
-- Code --

-- | The definition of the server itself.
server :: IORef Board -> ScottyM ()
server _ =
  get "/" $
    html "<h1>Nothing here yet.</h1>"

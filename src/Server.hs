module Server where

--------------------
-- Global Imports --
import Control.Monad.IO.Class
import Data.Monoid
import Web.Scotty

----------
-- Code --

-- | The definition of the server itself.
server :: ScottyM ()
server =
  get "/" $
    html "<h1>Nothing here yet.</h1>"

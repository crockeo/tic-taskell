-- | This module provides the API to server static files through Scotty.
module Static where

--------------------
-- Global Imports --
import qualified Data.Text.Lazy as LT
import qualified Data.Text      as T

import Control.Monad.IO.Class
import System.Directory
import Data.Monoid
import Network.Wai
import Web.Scotty

-------------------
-- Local Imports --
import Utils

----------
-- Code --

-- | Calculating the type of content to send back out.
calcContentType :: FilePath -> LT.Text
calcContentType =
  calcByExt . last . splitBy '.'
  where calcByExt :: String -> LT.Text
        calcByExt   "js" = "application/javascript"
        calcByExt  "css" = "text/css"
        calcByExt "html" = "text/html"
        calcByExt      _ = "text/plain"

-- | Serving static files if they exist.
serveStatic :: ScottyM ()
serveStatic =
  get (regex "^/static/(.*)") $ do
    path   <- fmap (T.unpack . foldl1 (\a b -> a <> "/" <> b) . pathInfo) request
    exists <- liftIO $ doesFileExist path
  
    if not exists
      then next
      else do
        setHeader "Content-Type" $ calcContentType path
        file path

{-# LANGUAGE TemplateHaskell #-}
module Server where

--------------------
-- Global Imports --
import Prelude hiding (error)

import Network.HTTP.Types.Status
import Control.Monad.IO.Class
import Data.Aeson.Types
import Control.Monad
import Data.Aeson.TH
import Data.IORef
import Web.Scotty
import Data.Char

-------------------
-- Local Imports --
import Static

import qualified Board as B (state)
import Board hiding (state)

----------
-- Code --

-- | A type to represent the @'Board'@ state.
data PullResponse = PullResponse { over   :: Bool
                                 , winner :: Int
                                 , turn   :: Int
                                 , state  :: [Int]
                                 }

$(deriveJSON defaultOptions { constructorTagModifier = map toLower }
             ''PullResponse)

-- | The type to represent a POST request to make a move.
data PushRequest = PushRequest { row :: Int
                               , col :: Int
                               }

$(deriveJSON defaultOptions { constructorTagModifier = map toLower }
             ''PushRequest)

-- | A type to represent the response to API push requests.
data PushResponse = PushResponse { error   :: Bool
                                 , msg     :: String
                                 , refresh :: Bool
                                 }

$(deriveJSON defaultOptions { constructorTagModifier = map toLower }
             ''PushResponse)

-- | Getting the home page.
getHomePage :: ScottyM ()
getHomePage =
  get "/" $ do
    status $ ok200
    setHeader "Content-Type" "text/html"
    file "views/home.html"

-- | Pulling the current state from the server.
getBoardState :: IORef Board -> ScottyM ()
getBoardState boardRef =
  get "/api/pull/state" $ do
    board <- liftIO $ readIORef boardRef

    json $ PullResponse { over   = isOver board
                        , winner = boardStateToInt $ findWinner board
                        , turn   = boardStateToInt $ determineTurn board
                        , state  = map boardStateToInt $ B.state board
                        }

-- | Dealing with a post request to reset the board.
postResetBoard :: IORef Board -> ScottyM ()
postResetBoard boardRef =
  post "/api/push/reset" $ do
    liftIO $ writeIORef boardRef defaultBoard

    json $ PushResponse { error   = False
                        , msg     = "Game reset!"
                        , refresh = True
                        }

-- | Dealing with a post request to perform a move.
postPerformMove :: IORef Board -> ScottyM ()
postPerformMove boardRef =
  post "/api/push/move" $ do
    postRequest <- jsonData :: ActionM PushRequest
    board <- liftIO $ readIORef boardRef

    let (done, board', message) = updateBoard ( ( row postRequest
                                                , col postRequest
                                                )
                                              , determineTurn board
                                              )
                                              board

    when done $
      liftIO $ writeIORef boardRef board'

    json $ PushResponse { error   = not done
                        , msg     = message
                        , refresh = True
                        }

-- | Responding to any request that doesn't get caught earlier on with a 404.
handle404 :: ScottyM ()
handle404 =
  notFound $ do
    status $ notFound404
    setHeader "Content-Type" "text/html"
    file "views/404.html"

-- | The definition of the server itself.
server :: IORef Board -> ScottyM ()
server boardRef = do
  serveStatic
  getHomePage

  getBoardState boardRef
  postResetBoard boardRef
  postPerformMove boardRef

  handle404

module Board where

--------------------
-- Global Imports --
import Control.Lens
import Data.Monoid
import Data.Maybe

-------------------
-- Local Imports --
import Utils

----------
-- Code --

-- | Defining the size of a board.
boardSize :: Int
boardSize = 3

-- | A type to represent a coordinate on the board (in the form of (row, col)).
type Point = (Int, Int)

-- | A datatype to represent the state of a board.
data BoardState = X
                | O
                | Nil
  deriving (Eq, Show, Read)

instance Monoid BoardState where
  mempty = Nil
  
  mappend   a Nil = a
  mappend Nil   a = a
  mappend   _   a = a

-- | Converting a @'BoardState'@ to a @'Char'@ to be displayed.
boardStateToChar :: BoardState -> Char
boardStateToChar   X = 'X'
boardStateToChar   O = 'O'
boardStateToChar Nil = '.'

-- | Converting a @'BoardState'@ to an @'Int'@ such that a minimax algorithm
--   can more easily be implemented.
boardStateToInt :: BoardState -> Int
boardStateToInt   X =  1
boardStateToInt   O = -1
boardStateToInt Nil =  0

-- | A type to represent an update to the @'Board'@.
type BoardUpdate = (Point, BoardState)

-- | The representation of the board itself.
data Board = Board { state :: [BoardState] }

-- | Displaying a board in a pretty fashion in text. Primarily meant for debug
--   purposes.
displayBoard :: Board -> String
displayBoard b =
  intersperse (map boardStateToChar $ state b) '\n' 3

-- | Converting a 2D coord to a 1D coord.
from2D :: Point -> Int
from2D (row, col) =
  row * boardSize + col

-- | Accessing an element in the @'Board'@ through a 2D coord.
boardPull :: Point -> Board -> BoardState
boardPull p b =
  state b ^. ix (from2D p)

-- | Checking if a location in the board is empty.
isEmpty :: Point -> Board -> Bool
isEmpty p b =
  boardPull p b == Nil

-- | Getting a list of valid locations to move.
validMoves :: Board -> [Point]
validMoves b =
  [(x, y) | x <- [0 .. boardSize - 1],
            y <- [0 .. boardSize - 1],
            isEmpty (x, y) b]

-- | Checking if a set of coordinates is a winner.
findWinnerCoords :: Board -> [Point] -> BoardState
findWinnerCoords b coords
  | length coords /= 3 = Nil
  | all (== X) vals    = X
  | all (== O) vals    = O
  | otherwise          = Nil
  where vals = for coords (\p -> boardPull p b)

-- | Generating a list of coordinates to check for a win.
winnableCoords :: [[Point]]
winnableCoords =
  generateRows ++
  generateCols ++
  generateDiags
  where generateRows  = [[(row, col) | col <- [0 .. boardSize - 1]] | row <- [0 .. boardSize - 1]]
        generateCols  = [[(row, col) | row <- [0 .. boardSize - 1]] | col <- [0 .. boardSize - 1]]
        generateDiags =
          [ [(0, 0), (1, 1), (2, 2)]
          , [(0, 2), (1, 1), (2, 0)]
          ]

-- | Finding the winner of a @'Board'@. If a winner does not yet exist, then it
--   simply returns @'Nothing'@.
findWinner :: Board -> BoardState
findWinner b
  | length winners == 0 = Nil
  | otherwise           = head winners
  where winners = filter (/= Nil) $ map (findWinnerCoords b) winnableCoords

-- | Checking if a game is finished.
isOver :: Board -> Bool
isOver b =
  findWinner b /= Nil || (length $ validMoves b) == 0

-- | Changing the state of a @'Board'@ at a given location.
boardPush :: (Int, Int) -> BoardState -> Board -> Board
boardPush p v b =
  Board $ state b & ix (from2D p) .~ v

-- | Determining the turn of the board.
determineTurn :: Board -> BoardState
determineTurn board
  | s == 0    = X
  | otherwise = O
  where s = sum $ map boardStateToInt $ state board

-- | Checking if a given move can be made on a board.
canMakeMove :: Point -> BoardState -> Board -> Bool
canMakeMove p v b
  | not $ isEmpty p b = False
  | isOver b                = False
  | v == Nil                = False
  | otherwise               = True

-- | Creating the default, empty, board.
defaultBoard :: Board
defaultBoard =
  Board $ replicate (boardSize * boardSize) Nil

-- | The function to updating a board.
updateBoard :: BoardUpdate -> Board -> (Bool, Board, String)
updateBoard (p, v) b =
  if not $ canMakeMove p v b
    then (False, b, "Cannot make that move!")
    else
      let b' = boardPush p v b
          mw = findWinner b' in
        case mw of
          Nil -> (True, b', "Move made!")
          w   -> (True, b', "Player " ++ [boardStateToChar w] ++ " has won!")

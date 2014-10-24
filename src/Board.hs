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
type BoardUpdate = (Int, Int, BoardState)

-- | The representation of the board itself.
data Board = Board { state :: [BoardState] }

-- | Displaying a board in a pretty fashion in text. Primarily meant for debug
--   purposes.
displayBoard :: Board -> String
displayBoard b =
  intersperse (map boardStateToChar $ state b) '\n' 3

-- | Converting a 2D coord to a 1D coord.
from2D :: Int -> Int -> Int
from2D row col =
  row * boardSize + col

-- | Accessing an element in the @'Board'@ through a 2D coord.
boardPull :: Int -> Int -> Board -> BoardState
boardPull row col b =
  state b ^. ix (from2D row col)

-- | Checking if a location in the board is empty.
isEmpty :: Int -> Int -> Board -> Bool
isEmpty row col b =
  boardPull row col b == Nil

-- | Getting a list of valid locations to move.
validMoves :: Board -> [(Int, Int)]
validMoves b =
  [(x, y) | x <- [0 .. boardSize - 1],
            y <- [0 .. boardSize - 1],
            isEmpty x y b]

-- | Checking if a set of coordinates is a winner.
findWinnerCoords :: Board -> [(Int, Int)] -> Maybe BoardState
findWinnerCoords b coords
  | length coords /= 3 = Nothing
  | otherwise =
    if all (== X) vals
      then Just X
      else if all (== O) vals
        then Just O
        else Nothing
  where vals = for coords (\(row, col) -> boardPull row col b)

-- | Generating a list of coordinates to check for a win.
winnableCoords :: [[(Int, Int)]]
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
findWinner :: Board -> Maybe BoardState
findWinner b
  | length winners == 0 = Nothing
  | otherwise           = Just $ head winners
  where winners = catMaybes $ map (findWinnerCoords b) winnableCoords

-- | Checking if a game is finished.
isOver :: Board -> Bool
isOver b =
  findWinner b /= Nothing || (length $ validMoves b) == 0

-- | Changing the state of a @'Board'@ at a given location.
boardPush :: Int -> Int -> BoardState -> Board -> Board
boardPush row col v b =
  Board $ state b & ix (from2D row col) .~ v

-- | Checking if a given move can be made on a board.
canMakeMove :: Int -> Int -> BoardState -> Board -> Bool
canMakeMove row col v b
  | not $ isEmpty row col b = False
  | isOver b                = False
  | v == Nil                = False
  | otherwise               = True

-- | Creating the default, empty, board.
defaultBoard :: Board
defaultBoard =
  Board $ replicate (boardSize * boardSize) Nil

-- | The function to updating a board.
updateBoard :: BoardUpdate -> Board -> (Board, String)
updateBoard (row, col, v) b =
  if not $ canMakeMove row col v b
    then (b, "Cannot make that move!")
    else
      let b' = boardPush row col v b
          mw = findWinner b' in
        case mw of
          Nothing -> (b', "Move made!")
          Just w  -> (b', "Player " ++ [boardStateToChar w] ++ " has won!")

module Board where

--------------------
-- Global Imports --
import FRP.Elerea.Simple
import Control.Lens
import Data.Monoid
import Data.String

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

-- | Finding the winner of a @'Board'@. If a winner does not yet exist, then it
--   simply returns @'Nothing'@.
findWinner :: Board -> Maybe BoardState
findWinner _ = Nothing

-- | Checking if a game is finished.
isOver :: Board -> Bool
isOver b =
  findWinner b /= Nothing || (length $ validMoves b) == 0

-- | Changing the state of a @'Board'@ at a given location.
boardPush :: Int -> Int -> BoardState -> Board -> Board
boardPush row col v b =
  Board $ state b & ix (from2D row col) .~ v

-- | Creating the default, empty, board.
defaultBoard :: Board
defaultBoard =
  Board $ replicate (boardSize * boardSize) Nil

-- | The function to updating a board.
updateBoard :: IsString a => BoardUpdate -> (Board, a) -> (Board, a)
updateBoard (  _,   _,  Nil)      p = p
updateBoard (row, col, move) (b, _)
  | not $ isEmpty row col b = (b, "Cannot move there!")
  | isOver b         = (b, "The game is already over!")
  | otherwise        = (boardPush row col move b, "Move performed!")

-- | The front-end for the @'Board' 'SignalGen'@.
boardSignalGenerator :: IsString a => Signal BoardUpdate -> SignalGen (Signal (Board, a))
boardSignalGenerator sBoardUpdate =
  transfer (defaultBoard, "") updateBoard sBoardUpdate

-- | This module contains the algorithm for determining the best possible move
--   in a game of tic-tac-toe.
module Minimax (findOptimalMove) where

-------------------
-- Local Imports --
import Board

----------
-- Code --

-- | Accessing the third element in a 3-tuple.
thd :: (a, b, c) -> c
thd (_, _, c) = c

-- | Finding the maximum score of a move.
maxScore :: BoardState -> Int
maxScore   X = -2
maxScore   O =  2
maxScore Nil =  0

-- | Finding the distance between two numbers.
distance :: Num a => a -> a -> a
distance a b = abs $ a - b

-- | Finding the better of two (Int, Int, Point)s.
findBetter :: Int -> (Int, Int, Point) -> (Int, Int, Point) -> (Int, Int, Point)
findBetter target (m1, s1, p1) (m2, s2, p2)
  | distance target s1 <  distance target s2 = (m1, s1, p1)
  | distance target s2 <  distance target s1 = (m2, s2, p1)
  | distance target s1 == distance target s2 =
    if m1 < m2
      then (m1, s1, p1)
      else (m2, s2, p2)

-- | The back-end to finding an optimal move in a board.
findOptimalMove' :: Int -> Point -> BoardState -> Board -> (Int, Int, Point)
findOptimalMove' numMoves pnt move board
  | isOver board = (numMoves, boardStateToInt $ findWinner board, pnt)
  | otherwise    =
    foldl1 (findBetter (maxScore move)) $
      map (\p -> findOptimalMove' (numMoves + 1) p (otherState move) (boardPush p move board)) moves
  where moves = validMoves board

-- | Finding the optimal move in a board.
findOptimalMove :: BoardState -> Board -> Point
findOptimalMove Nil = const (-1, -1)
findOptimalMove   s = thd . findOptimalMove' 0 (-1, -1) s

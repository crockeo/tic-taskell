-- | This module contains a set of utilities for use in other parts of the
--   project.
module Utils where

----------
-- Code --

-- | @'map'@, but with its arguments reversed.
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Splitting a map by a delimiter.
splitBy :: (Show a, Eq a) => a -> [a] -> [[a]]
splitBy =
  splitBy' []
  where splitBy' :: Eq a => [a] -> a -> [a] -> [[a]]
        splitBy' l' _     [] = [reverse l']
        splitBy' l' d (x:xs)
          | d == x    = (reverse l') : splitBy'     [] d xs
          | otherwise =                splitBy' (x:l') d xs

-- | Interspersing a value into an array at ever n indices.
intersperse :: [a] -> a -> Int -> [a]
intersperse =
  intersperse' 0
  where intersperse' :: Int -> [a] -> a -> Int -> [a]
        intersperse' _     []  _ _ = []
        intersperse' c (x:xs) x' n
          | c == n    = x' : intersperse'      0  (x:xs) x' n
          | otherwise = x  : intersperse' (c + 1)    xs  x' n

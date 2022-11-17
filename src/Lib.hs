module Lib
  ( someFunc,
  )
where

import Data.Function

smaller :: Ord a => [a] -> a -> [a]
smaller xs x = [a | a <- xs, a <= x]

larger :: Ord a => [a] -> a -> [a]
larger xs x = [a | a <- xs, a > x]

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort (smaller xs x) ++ [x] ++ qsort (larger xs x)

someFunc :: IO ()
someFunc =
  qsort ([2, 5, 4, 2, 5, 8, 1, 6, 4, 9] :: [Integer])
    & map show
    & unwords
    & print

module Utilities.Utils where

compareSort :: (a -> a -> Int) -> [a] -> [a]
compareSort _ [] = []
compareSort comparator (x:xs) = compareSort comparator left ++ [x] ++ compareSort comparator right
    where
        left = [y | y <- xs, comparator y x <= 0]
        right = [y | y <- xs, comparator y x > 0]
module Day1.Part1 ( main ) where

import Data.Char ( isDigit )

main :: IO ()
main = readFile "day1/input.txt" >>= print . sum . map readTwoDigit . lines

readTwoDigit :: String -> Int
readTwoDigit str = let ds = filter isDigit str
                   in  read [head ds, last ds]
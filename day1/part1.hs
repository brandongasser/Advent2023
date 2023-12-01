module Day1.Part1 ( main ) where

import Data.Char ( isDigit )

main :: IO ()
main = readFile "day1/input.txt" >>= print . sum . map readTwoDigit . lines

{-|
Reads the first digit and the last digit in a string as one two digit number

@
readTwoDigit "h3l1o" -> 31
readTwoDigit "he7lo" -> 77
@
-}
readTwoDigit :: String -> Int
readTwoDigit str = let ds = filter isDigit str
                   in  read [head ds, last ds]
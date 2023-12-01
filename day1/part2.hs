module Day1.Part2 ( main ) where

import Data.Char ( isDigit )
import Data.Text ( replace, pack, unpack )

main :: IO ()
main = readFile "day1/input.txt" >>= print . sum . map (readTwoDigit . replaceTextDigits) . lines

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

{-|
Replaces spelled out digits with its respective digit character, allowing overlapping.
Because of how overlapping is implemented, the result will contain spelled out digits.

@
replaceTextDigits "two1nine" -> "two2two1nine9nine"
replaceTextDigits "eightwothree" -> "eight8teightwo2twothree3three"
@
-}
replaceTextDigits :: String -> String
replaceTextDigits str = let digitStrings = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
                            pairs = [(pack x, pack (x ++ show i ++ x)) | (i, x) <- zip [1..] digitStrings]
                        in  unpack $ foldr (\(key, value) acc -> replace key value acc) (pack str) pairs
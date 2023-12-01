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

@
replaceTextDigits "two1nine" -> "219"
replaceTextDigits "eightwothree" -> "823"
@
-}
replaceTextDigits :: String -> String
replaceTextDigits str = let text1 = replace (pack "one") (pack "o1e") (pack str)
                            text2 = replace (pack "two") (pack "t2o") text1
                            text3 = replace (pack "three") (pack "t3e") text2
                            text4 = replace (pack "four") (pack "f4r") text3
                            text5 = replace (pack "five") (pack "f5e") text4
                            text6 = replace (pack "six") (pack "s6x") text5
                            text7 = replace (pack "seven") (pack "s7n") text6
                            text8 = replace (pack "eight") (pack "e8t") text7
                        in  unpack $ replace (pack "nine") (pack "n9e") text8
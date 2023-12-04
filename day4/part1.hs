module Day4.Part1 ( main ) where

import Day4.Parser ( parseFile, Card ( winningNumbers, cardNumbers ) )

main :: IO ()
main = do cards <- parseFile "day4/input.txt"
          print $ sum $ map score cards

score :: Card -> Int
score card = let wins = length $ filter (`elem` winningNumbers card) $ cardNumbers card
             in  if wins == 0 then 0 else 2 ^ (wins - 1)
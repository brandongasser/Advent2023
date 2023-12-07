module Day7.Part2 ( main ) where

import Day7.Parser2 ( Hand (Hand, cards, bid, handType), HandType (..), CardType (..), parseFile )
import Utilities.Utils ( compareSort )

compareHands :: Hand -> Hand -> Int
compareHands hand1 hand2 | handType hand1 /= handType hand2 = fromEnum (handType hand1) - fromEnum (handType hand2)
                         | otherwise = highCardWinner (cards hand1) (cards hand2)
    where
        highCardWinner (card1:cards1) (card2:cards2) | card1 == card2 = highCardWinner cards1 cards2
                                                     | otherwise      = fromEnum card1 - fromEnum card2

main :: IO ()
main = do hands <- parseFile "day7/input.txt"
          print $ sum $ zipWith (*) [1..] $ map bid $ compareSort compareHands hands
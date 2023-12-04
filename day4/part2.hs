module Day4.Part2 ( main ) where

import Day4.Parser ( parseFile, Card (cardId, winningNumbers, cardNumbers) )
import Control.Monad.State

handleCards :: Card -> State [Int] ()
handleCards originalCard = do currentCounts <- get
                              let winCount = wins originalCard
                              let winCards = [cardId originalCard + 1..cardId originalCard + winCount]
                              let addVector = take (length currentCounts) [if i `elem` winCards then currentCounts !! (cardId originalCard - 1) else 0 | i <- [1..]]
                              put $ addVectors currentCounts addVector

addVectors :: [Int] -> [Int] -> [Int]
addVectors = zipWith (+)

wins :: Card -> Int
wins card = length $ filter (`elem` winningNumbers card) $ cardNumbers card

main :: IO ()
main = do cards <- parseFile "day4/input.txt"
          let cardCounts = replicate (length cards) 1
          let result = execState (mapM_ handleCards cards) cardCounts
          print $ sum result
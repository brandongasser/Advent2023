module Day2.Part1 ( main ) where

import Day2.Parser ( Draw(Draw), Color(..), Game(..), parseFile )
import Data.Maybe ( fromJust )

counts :: [(Color, Int)]
counts = [(Red, 12), (Green, 13), (Blue, 14)]

main :: IO ()
main = do games <- parseFile "day2/input.txt"
          print $ sum $ map (\(Game id _) -> id) $ filter validGame games

validGame :: Game -> Bool
validGame (Game _ [])       = True
validGame (Game id (fd:fds)) = validFullDraw fd && validGame (Game id fds)
    where
        validFullDraw = all (\(Draw color n) -> fromJust (lookup color counts) >= n)
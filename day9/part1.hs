module Day9.Part1 ( main ) where

import Day9.Parser ( History, parseFile )

next :: History -> Int
next history = if   all (==0) history
               then 0
               else let differences = zipWith (-) (tail history) history
                        diff = next differences
                        current = last history
                    in  current + diff

main :: IO ()
main = do histories <- parseFile "day9/input.txt"
          print $ sum $ map next histories
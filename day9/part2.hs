module Day9.Part2 ( main ) where

import Day9.Parser ( History, parseFile )

prev :: History -> Int
prev history = if   all (==0) history
               then 0
               else let differences = zipWith (-) (tail history) history
                        diff = prev differences
                        current = head history
                    in  current - diff

main :: IO ()
main = do histories <- parseFile "day9/input.txt"
          print $ sum $ map prev histories
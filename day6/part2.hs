module Day6.Part2 where

data Race = Race {
    totalTime :: Int,
    totalDistance :: Int
} deriving Show

parseFile :: String -> IO Race
parseFile filename = (\[time, distance] -> Race time distance) . map (read . concat . tail . words) . lines <$> readFile filename

countPossibleWins :: Race -> Int
countPossibleWins race = let t = fromIntegral $ totalTime race
                             d = fromIntegral $ totalDistance race
                             leftBound = floor ((t + sqrt (t ^ 2 - 4 * d)) / 2) + 1
                             rightBound = ceiling ((t - sqrt (t ^ 2 - 4 * d)) / 2) - 1
                         in  leftBound - rightBound - 1

main :: IO ()
main = do race <- parseFile "day6/input.txt"
          print $ countPossibleWins race
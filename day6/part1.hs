module Day6.Part1 ( main ) where

data Race = Race {
    totalTime :: Int,
    totalDistance :: Int
} deriving Show

parseFile :: String -> IO [Race]
parseFile filename = (\[times, distances] -> zipWith Race times distances) . map (map read . tail . words) . lines <$> readFile filename

countPossibleWins :: Race -> Int
countPossibleWins race = let allDistances = [speed * (totalTime race - speed) | speed <- [0..totalTime race]]
                         in  length $ filter (>totalDistance race) allDistances

main :: IO ()
main = do races <- parseFile "day6/input.txt"
          print $ product $ map countPossibleWins races
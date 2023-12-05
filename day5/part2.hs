module Day5.Part2 ( main ) where

import Day5.Parser2 (Almanac (..), parseFile )

compress :: Almanac -> Int -> Int
compress almanac = soilToSeed almanac
                 . fertilizerToSoil almanac
                 . waterToFertilizer almanac
                 . lightToWater almanac
                 . temperatureToLight almanac
                 . humidityToTemperature almanac
                 . locationToHumidity almanac

inSeeds :: [(Int, Int)] -> Int -> Bool
inSeeds seeds x = any inSubRange seeds
    where
        inSubRange (start, range) = x >= start && x < start + range

main :: IO ()
main = do almanac <- parseFile "day5/input.txt"
          let pipeline = compress almanac
          print $ head $ dropWhile (not . inSeeds (seeds almanac) . pipeline) [1..]
module Day5.Part1 ( main ) where

import Day5.Parser ( Almanac(..), parseFile )

compress :: Almanac -> Int -> Int
compress almanac = humidityToLocation almanac
                 . temperatureToHumidity almanac
                 . lightToTemperature almanac
                 . waterToLight almanac
                 . fertilizerToWater almanac
                 . soilToFertilizer almanac
                 . seedToSoil almanac

main :: IO ()
main = do almanac <- parseFile "day5/input.txt"
          let pipeline = compress almanac
          print $ minimum $ map pipeline $ seeds almanac
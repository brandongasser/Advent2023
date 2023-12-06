module Day5.Part1 ( main ) where

import Day5.Parser ( Almanac(..), Range, parseFile )

compress :: Almanac -> [Range] -> [Range]
compress almanac = humidityToLocation almanac
                 . temperatureToHumidity almanac
                 . lightToTemperature almanac
                 . waterToLight almanac
                 . fertilizerToWater almanac
                 . soilToFertilizer almanac
                 . seedToSoil almanac

main :: IO ()
main = do almanac <- parseFile 1 "day5/input.txt"
          let pipeline = compress almanac
          print $ minimum $ pipeline (seeds almanac) >>= \(x, y) -> [x, y]
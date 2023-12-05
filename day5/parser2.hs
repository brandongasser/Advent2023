module Day5.Parser2 ( Almanac (..), parseFile ) where

import Utilities.Parse
    ( eol,
      nat,
      spaceChars,
      spaces,
      string,
      many1,
      parseFromFile,
      Parser )
import Data.List ( lookup )
import Data.Maybe ( fromMaybe )
import Control.Applicative ( (<|>) )

data Almanac = Almanac {
    seeds :: [(Int, Int)],
    soilToSeed :: Int -> Int,
    fertilizerToSoil :: Int -> Int,
    waterToFertilizer :: Int -> Int,
    lightToWater :: Int -> Int,
    temperatureToLight :: Int -> Int,
    humidityToTemperature :: Int -> Int,
    locationToHumidity :: Int -> Int
}

pseeds :: Parser [(Int, Int)]
pseeds = do string "seeds:"
            spaceChars
            many1 (do start <- nat
                      spaces
                      range <- nat
                      spaces
                      return (start, range))

pmapping :: Parser (Int -> Int)
pmapping = do ranges <- many1 (do sourceStart <- nat
                                  spaces
                                  destStart <- nat
                                  spaces
                                  range <- nat
                                  eol
                                  return (sourceStart, destStart, range))
              return $ lookupInRanges ranges
    where
        rangeValue (sourceStart, destStart, range) x = if   x >= sourceStart && x < sourceStart + range
                                                       then Just (destStart + x - sourceStart)
                                                       else Nothing
        lookupInRanges ranges x = fromMaybe x $ foldr (\current acc -> rangeValue current x <|> acc) Nothing ranges

pmap :: String -> Parser (Int -> Int)
pmap mapName = do string (mapName ++ " map:")
                  eol
                  pmapping

palmanac :: Parser Almanac
palmanac = do seeds <- pseeds
              spaces
              soilToSeed <- pmap "seed-to-soil"
              spaces
              fertilizerToSoil <- pmap "soil-to-fertilizer"
              spaces
              waterToFertilizer <- pmap "fertilizer-to-water"
              spaces
              lightToWater <- pmap "water-to-light"
              spaces
              temperatureToLight <- pmap "light-to-temperature"
              spaces
              humidityToTemperature <- pmap "temperature-to-humidity"
              spaces
              locationToHumidity <- pmap "humidity-to-location"
              return $ Almanac seeds soilToSeed fertilizerToSoil waterToFertilizer lightToWater temperatureToLight humidityToTemperature locationToHumidity

parseFile :: String -> IO Almanac
parseFile filename = parseFromFile palmanac filename >>= handle
    where
        handle result = case result of
                            Right x -> return x
                            Left x -> print x >> undefined
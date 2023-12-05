module Day5.Parser ( Almanac (seeds, seedToSoil, soilToFertilizer, fertilizerToWater, waterToLight, lightToTemperature, temperatureToHumidity, humidityToLocation), parseFile ) where

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
    seeds :: [Int],
    seedToSoil :: Int -> Int,
    soilToFertilizer :: Int -> Int,
    fertilizerToWater :: Int -> Int,
    waterToLight :: Int -> Int,
    lightToTemperature :: Int -> Int,
    temperatureToHumidity :: Int -> Int,
    humidityToLocation :: Int -> Int
}

pseeds :: Parser [Int]
pseeds = do string "seeds:"
            spaceChars
            many1 (nat <* spaceChars)

pmapping :: Parser (Int -> Int)
pmapping = do ranges <- many1 (do destStart <- nat
                                  spaces
                                  sourceStart <- nat
                                  spaces
                                  range <- nat
                                  eol
                                  return (destStart, sourceStart, range))
              return $ lookupInRanges ranges
    where
        rangeValue (destStart, sourceStart, range) x = if   x >= sourceStart && x < sourceStart + range
                                                       then Just (destStart + x - sourceStart)
                                                       else Nothing
        lookupInRanges ranges x = fromMaybe x $ foldr (\current acc -> rangeValue current x <|> acc) Nothing ranges
        -- lookupInRanges ranges x = let pairs = ranges >>= \(destStart, sourceStart, range) -> zip [sourceStart..sourceStart + range - 1] [destStart..destStart + range - 1]
        --                           in  fromMaybe x $ lookup x pairs

pmap :: String -> Parser (Int -> Int)
pmap mapName = do string (mapName ++ " map:")
                  eol
                  pmapping

palmanac :: Parser Almanac
palmanac = do seeds <- pseeds
              spaces
              seedToSoil <- pmap "seed-to-soil"
              spaces
              soilToFertilizer <- pmap "soil-to-fertilizer"
              spaces
              fertilizerToWater <- pmap "fertilizer-to-water"
              spaces
              waterToLight <- pmap "water-to-light"
              spaces
              lightToTemperature <- pmap "light-to-temperature"
              spaces
              temperatureToHumidity <- pmap "temperature-to-humidity"
              spaces
              humidityToLocation <- pmap "humidity-to-location"
              return $ Almanac seeds seedToSoil soilToFertilizer fertilizerToWater waterToLight lightToTemperature temperatureToHumidity humidityToLocation

parseFile :: String -> IO Almanac
parseFile filename = parseFromFile palmanac filename >>= handle
    where
        handle result = case result of
                            Right x -> return x
                            Left x -> print x >> undefined
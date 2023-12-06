{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Day5.Parser ( Almanac (..), Range, parseFile ) where

import Utilities.Parse
    ( eol,
      nat,
      spaceChars,
      spaces,
      string,
      many1,
      parseFromFile,
      Parser )
import Data.List ( sortOn )

type Range = (Int, Int)

data Almanac = Almanac {
    seeds :: [Range],
    seedToSoil :: [Range] -> [Range],
    soilToFertilizer :: [Range] -> [Range],
    fertilizerToWater :: [Range] -> [Range],
    waterToLight :: [Range] -> [Range],
    lightToTemperature :: [Range] -> [Range],
    temperatureToHumidity :: [Range] -> [Range],
    humidityToLocation :: [Range] -> [Range]
}

ppart1seeds :: Parser [Range]
ppart1seeds = do string "seeds:"
                 spaceChars
                 ns <- many1 (nat <* spaceChars)
                 return $ map (\n -> (n, n)) ns

ppart2seeds :: Parser [Range]
ppart2seeds = do string "seeds:"
                 spaceChars
                 many1 (do start <- nat
                           spaces
                           range <- nat
                           spaces
                           return (start, start + range - 1))

pseeds :: Int -> Parser [Range]
pseeds partNumber = if partNumber == 1 then ppart1seeds else ppart2seeds

pmapping :: Parser ([Range] -> [Range])
pmapping = do ranges <- many1 (do destStart <- nat
                                  spaces
                                  sourceStart <- nat
                                  spaces
                                  range <- nat
                                  eol
                                  return (sourceStart, sourceStart + range - 1, destStart, destStart + range - 1))
              return $ mapRanges ranges
    where
        mapRanges mapperRanges inputRanges = let sortedMapperRanges = sortOn (\(x,_,_,_) -> x) mapperRanges
                                             in inputRanges >>= resultRanges sortedMapperRanges
        resultRanges [] inputRange = [inputRange]
        resultRanges ((mapperSourceStart, mapperSourceEnd, mapperDestStart, mapperDestEnd):mapperRanges) (inputStart, inputEnd)
               | inputEnd < mapperSourceStart = [(inputStart, inputEnd)]
               | inputStart > mapperSourceEnd = resultRanges mapperRanges (inputStart, inputEnd)
               | inputStart < mapperSourceStart = (inputStart, mapperSourceStart - 1) : resultRanges ((mapperSourceStart, mapperSourceEnd, mapperDestStart, mapperDestEnd):mapperRanges) (mapperSourceStart, inputEnd)
               | inputEnd <= mapperSourceEnd = [(mapperDestStart + (inputStart - mapperSourceStart), mapperDestEnd - (mapperSourceEnd - inputEnd))]
               | inputEnd > mapperSourceEnd = (mapperDestStart + (inputStart - mapperSourceStart), mapperDestEnd) : resultRanges mapperRanges (mapperSourceEnd + 1, inputEnd)

pmap :: String -> Parser ([Range] -> [Range])
pmap mapName = do string (mapName ++ " map:")
                  eol
                  pmapping

palmanac :: Int -> Parser Almanac
palmanac partNumber = do seeds <- pseeds partNumber
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

parseFile :: Int -> String -> IO Almanac
parseFile partNumber filename = parseFromFile (palmanac partNumber) filename >>= handle
    where
        handle result = case result of
                            Right x -> return x
                            Left x -> print x >> undefined
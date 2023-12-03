{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Day3.Part1 ( main ) where

import Data.Char ( isDigit )
import Data.List ( groupBy )

isSymbol :: Char -> Bool
isSymbol x = not (isDigit x) && x /= '.'

nextToSymbol :: (Int, Int) -> [[Char]] -> Bool
nextToSymbol (y, x) schematic = any isSymbol [(schematic !! y') !! x' | (y', x') <- adjacentPoses]
    where
        adjacentPoses = filter (\(y', x') -> (y', x') /= (y, x) && y' >= 0 && y' < length schematic && x' >= 0 && x' < length (head schematic)) [(y + dy, x + dx) | dy <- [-1..1], dx <- [-1..1]]

rowSum :: Int -> [[Char]] -> Int
rowSum y schematic = let goodGroups = filter (any (`nextToSymbol` schematic)) numPoses
                         numStrings = map (map (\(y', x') -> (schematic !! y') !! x')) goodGroups
                     in  sum $ map read numStrings
    where
        row = schematic !! y
        numIndexes = makeGroups [i | (i, z) <- zip [0..] row, isDigit z]
        makeGroups ns = let ns' = zip [0..] ns
                        in  map (map snd) $ groupBy (\(i1, n1) (i2, n2) -> n2 - n1 == i2 - i1) ns'
        numPoses = map (map (\x -> (y, x))) numIndexes

main :: IO ()
main = do schematic <- lines <$> readFile "day3/input.txt"
          print $ sum $ map (`rowSum` schematic) [0..length schematic - 1]
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Day3.Part2 ( main ) where

import Data.Char ( isDigit )
import Data.List ( groupBy, sortOn )

surroundingDigitPoses :: (Int, Int) -> [[Char]] -> [(Int, Int)]
surroundingDigitPoses (y, x) schematic = filter (\(y', x') -> isDigit ((schematic !! y') !! x')) adjacentPoses
    where
        adjacentPoses = filter (\(y', x') -> (y', x') /= (y, x) && y' >= 0 && y' < length schematic && x' >= 0 && x' < length (head schematic)) [(y + dy, x + dx) | dy <- [-1..1], dx <- [-1..1]]

sanitizeRow :: Int -> [[Char]] -> [Char]
sanitizeRow y schematic = let adjust c i | i `elem` gearIndexes = '*'
                                         | isDigit c            = c
                                         | otherwise            = '.'
                          in  [adjust c i | (i, c) <- zip [0..] row]
    where
        row = schematic !! y
        gearIndexes = map snd $ filter isGoodGear [(y, x) | (x, z) <- zip [0..] row, z == '*']
        isGoodGear gearPose = let surrounding = surroundingDigitPoses gearPose schematic
                              in  (==2) $ length $ groupBy (\(numY1, numX1) (numY2, numX2) -> numY1 == numY2 && all (`elem` surrounding) [(numY1, b) | b <- [min numX1 numX2 + 1..max numX1 numX2 - 1]]) $ sortOn fst surrounding

sanitize :: [[Char]] -> [[Char]]
sanitize schematic = map (`sanitizeRow` schematic) [0..length schematic - 1]

rowProduct :: (Int, Int) -> Int -> [[Char]] -> Integer
rowProduct (gearY, gearX) y schematic = let goodGroups = filter (any nextToGear) numPoses
                                            numStrings = map (map (\(y', x') -> (schematic !! y') !! x')) goodGroups
                                        in  product $ map read numStrings
    where
        row = schematic !! y
        numIndexes = makeGroups [i | (i, z) <- zip [0..] row, isDigit z]
        makeGroups ns = let ns' = zip [0..] ns
                        in  map (map snd) $ groupBy (\(i1, n1) (i2, n2) -> n2 - n1 == i2 - i1) ns'
        numPoses = map (map (\x -> (y, x))) numIndexes
        nextToGear (y', x') = max (abs (gearY - y')) (abs (gearX - x')) == 1

main :: IO ()
main = do schematic <- lines <$> readFile "day3/input.txt"
          let schematic' = sanitize schematic
          let gearPoses = [(y, x) | (y, row) <- zip [0..] schematic', (x, c) <- zip [0..] row, c == '*']
          print $ sum $ map (\gearPose -> product $ map (\y -> rowProduct gearPose y schematic') [0..length schematic - 1]) gearPoses
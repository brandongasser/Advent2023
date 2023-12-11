module Day10.Parser ( parseFile, Pipe (..), Pose, PipeMap ) where

import Data.Maybe ( fromJust, isJust )

data Pipe = Start | Vertical | Horizontal | TopLeft | TopRight | BottomLeft | BottomRight
    deriving (Show, Eq)

type Pose = (Int, Int)

type PipeMap = [(Pose, Pipe)]

readPipe :: Char -> Maybe Pipe
readPipe = flip lookup (zip "S|-F7LJ" [Start, Vertical, Horizontal, TopLeft, TopRight, BottomLeft, BottomRight])

parseFile :: String -> IO PipeMap
parseFile filename = do input <- readFile filename
                        return [((x, y), fromJust $ readPipe c) | (y, line) <- zip [0..] (lines input), (x, c) <- zip [0..] line, isJust $ readPipe c]
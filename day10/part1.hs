module Day10.Part1 ( main ) where

import Day10.Parser ( parseFile, Pipe (..), Pose, PipeMap )

connected :: Pose -> PipeMap -> [Pose]
connected pose pipeMap = let pipe = lookup pose pipeMap
                         in  maybe [] findConnected pipe
    where
      findConnected Start = filter (elem pose . (`connected` pipeMap)) [(fst pose + 1, snd pose), (fst pose - 1, snd pose), (fst pose, snd pose + 1), (fst pose, snd pose - 1)]
      findConnected Vertical = [(fst pose, snd pose + 1), (fst pose, snd pose - 1)]
      findConnected Horizontal = [(fst pose + 1, snd pose), (fst pose - 1, snd pose)]
      findConnected TopLeft = [(fst pose + 1, snd pose), (fst pose, snd pose + 1)]
      findConnected TopRight = [(fst pose - 1, snd pose), (fst pose, snd pose + 1)]
      findConnected BottomLeft = [(fst pose + 1, snd pose), (fst pose, snd pose - 1)]
      findConnected BottomRight = [(fst pose - 1, snd pose), (fst pose, snd pose - 1)]

findLoop :: [Pose] -> PipeMap -> [Pose]
findLoop (current:visited) pipeMap = let nextOptions = filter (not . (`elem` visited)) $ connected current pipeMap
                                     in  if   null nextOptions
                                         then current:visited
                                         else findLoop (head nextOptions:current:visited) pipeMap

main :: IO ()
main = do pipeMap <- parseFile "day10/input.txt"
          let startPose = fst $ head $ filter ((==Start) . snd) pipeMap
          print $ (`div`2) $ length $ findLoop [startPose] pipeMap
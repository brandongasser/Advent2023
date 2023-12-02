module Day2.Part2 ( main ) where

import Day2.Parser ( Game(..), Draw(Draw), Color(Blue, Red, Green), parseFile )

main :: IO ()
main = do games <- parseFile "day2/input.txt"
          print $ sum $ map gamePower games

gamePower :: Game -> Int
gamePower (Game _ fds) = colorMax Red * colorMax Green * colorMax Blue
    where
        flatFds = concat fds
        colorMax color = maximum $ map (\(Draw _ n) -> n) $ filter (\(Draw c _) -> c == color) flatFds
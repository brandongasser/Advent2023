module Day2.Parser ( Color (Red, Green, Blue), Draw (Draw), FullDraw, Game (Game), parseFile ) where

import Utilities.Parse
    ( eol,
      nat,
      char,
      spaces,
      string,
      many1,
      (<|>),
      many,
      parseFromFile,
      Parser )

data Color = Red | Green | Blue
    deriving (Show, Eq)

color :: Parser Color
color = (string "red" >> return Red) <|> (string "green" >> return Green) <|> (string "blue" >> return Blue)

data Draw = Draw Color Int
    deriving Show

draw :: Parser Draw
draw = do n <- nat
          spaces
          flip Draw n <$> color

type FullDraw = [Draw]

fullDraw :: Parser FullDraw
fullDraw = many1 (do d <- draw
                     many (char ',')
                     many (char ' ')
                     return d)

data Game = Game Int [FullDraw]
    deriving Show

game :: Parser Game
game = do string "Game "
          id <- nat
          string ": "
          fds <- many1 (do fd <- fullDraw
                           many (char ';')
                           many (char ' ')
                           return fd)
          return $ Game id fds

parseFile :: String -> IO [Game]
parseFile filename = parseFromFile (many1 (game <* eol)) filename >>= \(Right result) -> return result
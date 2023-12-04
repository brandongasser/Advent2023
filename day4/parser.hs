module Day4.Parser ( Card (Card, id, winningNumbers, cardNumbers), parseFile ) where

import Utilities.Parse
    ( char,
      string,
      many1,
      parseFromFile,
      Parser,
      eol,
      nat,
      spaceChars )

numList :: Parser [Int]
numList = many1 (nat <* spaceChars)

data Card = Card {
    id :: Int,
    winningNumbers :: [Int],
    cardNumbers :: [Int]
} deriving Show

card :: Parser Card
card = do string "Card"
          spaceChars
          id <- nat
          char ':'
          spaceChars
          winningNumbers <- numList
          spaceChars
          char '|'
          spaceChars
          Card id winningNumbers <$> numList

parseFile :: String -> IO [Card]
parseFile filename = parseFromFile (many1 (card <* eol)) filename >>= \(Right result) -> return result
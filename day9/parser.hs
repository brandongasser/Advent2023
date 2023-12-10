module Day9.Parser ( History, parseFile ) where

import Utilities.Parse ( spaceChars, many1, parseFromFile, Parser, eol, int )

type History = [Int]

history :: Parser History
history = many1 (int <* spaceChars)

parseFile :: String -> IO [History]
parseFile filename = parseFromFile (many1 (history <* eol)) filename >>= \(Right result) -> return result
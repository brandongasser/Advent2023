module Day8.Parser ( Direction (L, R), Node ( Node, name, nodeNameOn ), parseFile ) where

import Utilities.Parse ( char, anyChar, spaces, many1, (<|>), parseFromFile, Parser, eol )
import Control.Monad ( replicateM )

data Direction = L | R
    deriving (Show, Eq)

data Node = Node {
    name :: String,
    nodeNameOn :: Direction -> String
}

direction :: Parser Direction
direction = (char 'L' >> return L) <|> (char 'R' >> return R)

nodeName :: Parser String
nodeName = replicateM 3 anyChar

node :: Parser Node
node = do name <- nodeName
          spaces
          char '='
          spaces
          char '('
          leftName <- nodeName
          char ','
          spaces
          rightName <- nodeName
          char ')'
          return $ Node name (\d -> if d == L then leftName else rightName)

allInput :: Parser ([Direction], [Node])
allInput = do directions <- cycle <$> many1 direction
              spaces
              nodes <- many1 (node <* eol)
              return (directions, nodes)

parseFile :: String -> IO ([Direction], [Node])
parseFile filename = parseFromFile allInput filename >>= \(Right result) -> return result
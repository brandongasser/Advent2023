module Day8.Part2 ( main ) where

import Day8.Parser ( Direction (L, R), Node (Node, name, nodeNameOn), parseFile )
import Control.Monad.State ( MonadState(put, get), evalState, State )

countTurns :: [Direction] -> [Node] -> State Node Int
countTurns (direction:directions) nodes = do currentNode <- get
                                             let nextNodeName = currentNode `nodeNameOn` direction
                                             let nextNode = head $ filter ((==nextNodeName) . name) nodes
                                             put nextNode
                                             if   last (name currentNode) == 'Z'
                                             then return 0
                                             else (+1) <$> countTurns directions nodes

main :: IO ()
main = do (directions, nodes) <- parseFile "day8/input.txt"
          let results = map (evalState (countTurns directions nodes)) $ filter ((=='A') . last . name) nodes
          print $ foldr lcm 1 results
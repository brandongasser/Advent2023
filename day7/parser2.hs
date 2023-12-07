module Day7.Parser2 where

import Utilities.Parse ( char, spaces, many1, (<|>), parseFromFile, Parser, eol, nat )
import Control.Monad ( replicateM )
import Data.List ( group, sort )

data HandType = HighCard | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
    deriving (Show, Eq, Ord, Enum)

data CardType = Jack | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Queen | King | Ace
    deriving (Show, Eq, Ord, Enum)

data Hand = Hand {
    cards :: [CardType],
    bid :: Int,
    handType :: HandType
} deriving Show

cardType :: Parser CardType
cardType =   builder '2' Two
         <|> builder '3' Three
         <|> builder '4' Four
         <|> builder '5' Five
         <|> builder '6' Six
         <|> builder '7' Seven
         <|> builder '8' Eight
         <|> builder '9' Nine
         <|> builder 'T' Ten
         <|> builder 'J' Jack
         <|> builder 'Q' Queen
         <|> builder 'K' King
         <|> builder 'A' Ace
    where
        builder c t = char c >> return t

determineHandType :: [CardType] -> HandType
determineHandType cards = let jacks = length $ filter (==Jack) cards
                              cardGroupSizes = (\counts -> (last counts + jacks) : init counts) $ (0:) $ sort $ map length $ group $ sort $ filter (/=Jack) cards
                              pairs = length $ filter (==2) cardGroupSizes
                              threeKinds = length $ filter (==3) cardGroupSizes
                              fourKinds = length $ filter (==4) cardGroupSizes
                              fiveKinds = length $ filter (==5) cardGroupSizes
                          in  aux pairs threeKinds fourKinds fiveKinds
    where
        aux pairs threeKinds fourKinds fiveKinds 
            | fiveKinds == 1                = FiveKind
            | fourKinds == 1                = FourKind
            | threeKinds == 1 && pairs == 1 = FullHouse
            | threeKinds == 1               = ThreeKind
            | pairs == 2                    = TwoPair
            | pairs == 1                    = OnePair
            | otherwise                     = HighCard

hand :: Parser Hand
hand = do cards <- replicateM 5 cardType
          spaces
          bid <- nat
          let handType = determineHandType cards
          return $ Hand cards bid handType

parseFile :: String -> IO [Hand]
parseFile filename = parseFromFile (many1 (hand <* eol)) filename >>= \(Right result) -> return result
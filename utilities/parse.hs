module Utilities.Parse ( module Text.Parsec, module Text.Parsec.String, eol, nat, int, decimal ) where

import Text.Parsec
import Text.Parsec.String

import Control.Monad ( void )

eol :: Parser ()
eol = void newline <|> eof

nat :: Parser Int
nat = read <$> many1 digit

int :: Parser Int
int = (char '-' >> (*(-1)) <$> nat) <|> nat

decimal :: Parser Double
decimal = try (do whole <- int
                  char '.'
                  dec <- nat
                  return $ read (show whole ++ "." ++ show dec))
               <|> fromIntegral <$> int
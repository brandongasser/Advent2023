module Utilities.Tree ( Tree (Node) ) where

data Tree a = Node a [Tree a]
    deriving (Show, Eq, Read, Foldable)

instance Functor Tree where
    fmap f (Node x subs) = Node (f x) (map (fmap f) subs)

instance Applicative Tree where
    pure x = let t = Node x (repeat t) in t
    (Node f fsubs) <*> (Node x xsubs) = Node (f x) (zipWith (<*>) fsubs xsubs)
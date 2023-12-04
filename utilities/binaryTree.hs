module Utilities.BinaryTree ( BinaryTree (Node, Leaf) ) where

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Leaf
    deriving (Show, Eq, Read, Foldable)

instance Functor BinaryTree where
    fmap _ Leaf = Leaf
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Applicative BinaryTree where
    pure x = let t = Node x t t in t
    (Node f lf rf) <*> (Node x lx rx) = Node (f x) (lf <*> lx) (rf <*> rx)
    _              <*> _              = Leaf
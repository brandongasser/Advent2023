module Utilities.Stack ( Stack, empty, fromList, toList, push, pushAll, pop, popMany ) where

newtype Stack a = Stack [a]
    deriving (Read, Show, Foldable)

instance Functor Stack where
    fmap f (Stack xs) = Stack (map f xs)

instance Applicative Stack where
    pure x = Stack $ pure x
    (Stack xs) <*> Stack ys = Stack (xs <*> ys)

instance Monad Stack where
    (Stack xs) >>= f = foldr ((\(Stack ys) (Stack acc) -> Stack (ys ++ acc)) . f) (Stack []) xs

empty :: Stack a
empty = fromList []

fromList :: [a] -> Stack a
fromList = Stack

toList :: Stack a -> [a]
toList (Stack xs) = xs

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pushAll :: [a] -> Stack a -> Stack a
pushAll xs stack = foldl (flip push) stack xs

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

popMany :: Int -> Stack a -> ([Maybe a], Stack a)
popMany n stack | n <= 0 =    ([], stack)
                | otherwise = let (x, stack')   = pop stack
                                  (xs, stack'') = popMany (n - 1) stack'
                              in  (x:xs, stack'')
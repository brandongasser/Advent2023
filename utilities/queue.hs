module Utilities.Queue ( Queue, empty, fromList, toList, enqueue, enqueueAll, dequeue, dequeueMany ) where

data Queue a = Queue {
    inbox :: [a],
    outbox :: [a]
} deriving (Foldable)

instance (Show a) => Show (Queue a) where
    show queue = "Queue " ++ show (inbox queue ++ outbox queue)

instance Functor Queue where
    fmap f queue = Queue { inbox = map f $ inbox queue, outbox = map f $ outbox queue }

instance Applicative Queue where
    pure x = Queue { inbox = [], outbox = pure x }
    q1 <*> q2 = Queue { inbox = [], outbox = toList q1 <*> toList q2 }

instance Monad Queue where
    queue >>= f = Queue { inbox = inbox queue >>= reverse . toList . f, outbox = outbox queue >>= toList . f }

empty :: Queue a
empty = fromList []

fromList :: [a] -> Queue a
fromList = Queue []

toList :: Queue a -> [a]
toList queue = inbox queue ++ reverse (outbox queue)

enqueue :: a -> Queue a -> Queue a
enqueue x queue = Queue { inbox = x : inbox queue, outbox = outbox queue }

enqueueAll :: [a] -> Queue a -> Queue a
enqueueAll xs queue = foldl (flip enqueue) queue xs

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue queue | null (outbox queue) && null (inbox queue) = (Nothing, queue)
              | null (outbox queue) = dequeue $ Queue { inbox = [], outbox = reverse $ inbox queue }
              | otherwise = (Just $ head $ outbox queue, Queue { inbox = inbox queue, outbox = tail $ outbox queue })

dequeueMany :: Int -> Queue a -> ([Maybe a], Queue a)
dequeueMany n queue | n <= 0 =    ([], queue)
                    | otherwise = let (x, queue') = dequeue queue
                                      (xs, queue'') = dequeueMany (n - 1) queue'
                                  in  (x:xs, queue'')
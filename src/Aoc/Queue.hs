module Aoc.Queue where

import Data.Hashable (Hashable (..))

data Queue a = Queue [a] [a] deriving (Show)

instance Eq a => Eq (Queue a) where
  a == b = pop a == pop b

instance Ord a => Ord (Queue a) where
  compare a b = compare (pop a) (pop b)

instance Hashable a => Hashable (Queue a) where
  hashWithSalt i q = case pop q of
    Nothing -> i
    Just (a, q') -> i `hashWithSalt` a `hashWithSalt` q'

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] _) = Nothing
pop q = Just $ popUnsafe q

popUnsafe :: Queue a -> (a, Queue a)
popUnsafe (Queue (t : ts) b) = (t, refill $ Queue ts b)

refill :: Queue a -> Queue a
refill (Queue [] b) = Queue (reverse b) []
refill q = q

push :: a -> Queue a -> Queue a
push c (Queue [] _) = Queue [c] []
push c (Queue t b) = Queue t (c : b)

isEmpty :: Queue a -> Bool
isEmpty (Queue [] _) = True
isEmpty _ = False

size :: Queue a -> Int
size (Queue t b) = length t + length b

take :: Int -> Queue a -> Queue a
take i q = fromList (Prelude.take i $ toList q)

fromList :: [a] -> Queue a
fromList list = Queue list []

toList :: Queue a -> [a]
toList (Queue t b) = t ++ reverse b

toListR :: Queue a -> [a]
toListR (Queue t b) = b ++ reverse t
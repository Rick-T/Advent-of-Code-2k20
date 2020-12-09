module Aoc.Util where

import Data.Foldable (Foldable (foldl'))

countMatches :: Foldable f => (a -> Bool) -> f a -> Int
countMatches condition = foldl' (\c a -> if condition a then c + 1 else c) 0

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

minMax :: (Foldable t, Ord a, Bounded a) => t a -> (a, a)
minMax = foldr (\a (x, y) -> (min x a, max y a)) (maxBound, minBound)
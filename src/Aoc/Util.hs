module Aoc.Util where

import Data.Foldable (toList)

countMatches :: Foldable f => (a -> Bool) -> f a -> Int
countMatches condition = length . filter condition . toList

hasAtLeast :: Foldable f => Int -> (a -> Bool) -> f a -> Bool
hasAtLeast target condition
  | target <= 0 = const True
  | otherwise = go target . toList
  where
    go 0 _ = True
    go _ [] = False
    go i (l : ls) = if condition l then go (i -1) ls else go i ls

hasAtMost :: Foldable f => Int -> (a -> Bool) -> f a -> Bool
hasAtMost target condition
  | target < 0 = const False
  | otherwise = go (target + 1) . toList
  where
    go 0 _ = False
    go _ [] = True
    go i (l : ls) = if condition l then go (i -1) ls else go i ls

hasExactly :: Foldable f => Int -> (a -> Bool) -> f a -> Bool
hasExactly target condition = (== target) . countMatches condition

firstEqual :: Eq x => [x] -> x
firstEqual [] = undefined
firstEqual [x] = x
firstEqual (x : y : xs) = if x == y then x else firstEqual (y : xs)

iterateN :: Int -> (a -> a) -> a -> a
iterateN i f = (!! i) . iterate f

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

minMax :: (Foldable t, Ord a, Bounded a) => t a -> (a, a)
minMax = foldr (\a (x, y) -> (min x a, max y a)) (maxBound, minBound)
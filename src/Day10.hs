module Day10 where

import Aoc.Input (readInputList, withInput)
import Data.List (sort)

-- >>> part1
-- 2201
part1 :: IO Int
part1 = withInput "Day10.txt" (readInputList read) solvePart1

-- >>> part2
-- 169255295254528
part2 :: IO Int
part2 = withInput "Day10.txt" (readInputList read) solvePart2

solvePart1 :: [Int] -> Int
solvePart1 l = go (0 : sort l) 0 0
  where
    go [] _ _ = 0
    go [_] d1 d3 = d1 * (d3 + 1)
    go (x : y : r) d1 d3
      | x + 1 == y = go (y : r) (d1 + 1) d3
      | x + 2 == y = go (y : r) d1 d3
      | x + 3 == y = go (y : r) d1 $ d3 + 1
      | otherwise = go [] d1 (d3 + 1)

solvePart2 :: [Int] -> Int
solvePart2 ls = go (1 : repeat 0) (sort ls) 0
  where
    go [] [] _ = 0
    go as [] _ = head as
    go as (l : ls) i =
      let diff = (l - i -1)
          s = sum $ take (3 - diff) as
          as' = foldr (:) as (s : replicate diff 0)
       in go as' ls l
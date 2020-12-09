module Day09 where

import Aoc.Input (readInput, withInput)
import Aoc.Util (minMax)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

-- >>> part1
-- 373803594
part1 :: IO Int
part1 = withInput "Day09.txt" (readInput toVector) solvePart1

-- >>> part2
-- 51152360
part2 :: IO Int
part2 = withInput "Day09.txt" (readInput toVector) solvePart2

solvePart1 :: Vector Int -> Int
solvePart1 xs =
  let x = xs ! 25
      pairSums = [x + y | x <- (xs !) <$> [0 .. 24], y <- (xs !) <$> [0 .. 24], x /= y]
   in if x `elem` pairSums then solvePart1 (V.tail xs) else x

solvePart2 :: Vector Int -> Int
solvePart2 xs =
  let target = solvePart1 xs
      (x, y) = minMax $ contiguousTo target xs
   in x + y

contiguousTo :: Int -> Vector Int -> Vector Int
contiguousTo target xs = go 0 0 0
  where
    go low high acc
      | acc < target = go low (high + 1) (acc + xs ! high)
      | acc > target = go (low + 1) high (acc - xs ! low)
      | otherwise = V.slice low (high - low) xs

toVector :: String -> Vector Int
toVector = V.fromList . fmap read . lines
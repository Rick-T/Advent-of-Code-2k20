module Day01 where

import Aoc.Input (readInputList, withInput)

part1 :: IO Int
part1 = withInput "Day01.txt" (readInputList read) solvePart1

part2 :: IO Int
part2 = withInput "Day01.txt" (readInputList read) solvePart2

solvePart1 :: [Int] -> Int
solvePart1 input = head [x * y | x <- input, y <- input, x + y == 2020]

solvePart2 :: [Int] -> Int
solvePart2 input = head [x * y * z | x <- input, y <- input, z <- input, x + y + z == 2020]
module Day05 where

import Aoc.Input (readInputListParsed, withInput)
import Data.List (foldl', sort)

part1 :: IO Int
part1 = withInput "Day05.txt" (readInputListParsed parseSeatId) maximum

part2 :: IO Int
part2 = withInput "Day05.txt" (readInputListParsed parseSeatId) solvePart2

solvePart2 :: [Int] -> Int
solvePart2 = findSeat . sort

findSeat :: [Int] -> Int
findSeat [] = error "Dude where is my seat?"
findSeat [_] = error "Dude where is my seat?"
findSeat (x : y : r)
  | x + 1 == y = findSeat (y : r)
  | otherwise = x + 1

parseSeatId :: String -> Int
parseSeatId = foldl' (\a b -> 2 * a + b) 0 . fmap asDigit

asDigit :: Char -> Int
asDigit 'F' = 0
asDigit 'B' = 1
asDigit 'L' = 0
asDigit 'R' = 1
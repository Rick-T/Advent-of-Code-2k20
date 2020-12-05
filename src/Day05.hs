module Day05 where

import Aoc.Input (readInputListParsed, withInput)
import Data.List (foldl', sort)

data Search = Search Int Int Int Int deriving (Show)

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

initialSearch :: Search
initialSearch = Search 0 127 0 7

parseSeatId :: String -> Int
parseSeatId = seatId . runSearch

seatId :: Search -> Int
seatId (Search x y v w)
  | x == y && v == w = 8 * x + v
  | otherwise = error "Search did not finish"

runSearch :: String -> Search
runSearch = foldl' searchStep initialSearch

searchStep :: Search -> Char -> Search
searchStep (Search x y v w) c = case c of
  'F' -> Search x (center x y) v w
  'B' -> Search (center x y + 1) y v w
  'L' -> Search x y v (center v w)
  'R' -> Search x y (center v w + 1) w

center :: Int -> Int -> Int
center lower higher = lower + ((higher - lower) `div` 2)
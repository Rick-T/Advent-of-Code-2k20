module Day06 where

import Aoc.Input (readInputParsed, withInput)
import Data.Set (Set, insert)

type Questions = Set Char

part1 :: IO Int
part1 = withInput "Day06.txt" (readInputParsed readQuestions) solvePart1

part2 :: IO Int
part2 = withInput "Day06.txt" (readInputParsed readQuestions) solvePart2

solvePart1 :: [Set String] -> Int
solvePart1 = sum . fmap (countAnswers any)

solvePart2 :: [Set String] -> Int
solvePart2 = sum . fmap (countAnswers all)

countAnswers :: ((String -> Bool) -> Set String -> Bool) -> Set String -> Int
countAnswers f s = length [c | c <- ['a' .. 'z'], f (elem c) s]

readQuestions :: String -> [Set String]
readQuestions string = go (lines string) mempty []
  where
    go [] set list = set : list
    go (l : ls) set list = if l == "" then go ls mempty (set : list) else go ls (insert l set) list
module Aoc.Input where

import Paths_Advent_of_Code_2k20 (getDataFileName)

readPuzzleInput :: String -> IO String
readPuzzleInput filename = readFile =<< getDataFileName ("input/" ++ filename)

readInput :: (String -> a) -> String -> IO a
readInput morph filename = morph <$> readPuzzleInput filename

readInputList :: (String -> a) -> String -> IO [a]
readInputList morph filename = fmap morph . lines <$> readPuzzleInput filename

withInput :: String -> (String -> IO a) -> (a -> b) -> IO b
withInput filename reader f = f <$> reader filename
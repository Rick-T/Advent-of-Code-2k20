module Aoc.Input where

import Paths_Advent_of_Code_2k20 (getDataFileName)

readInput :: String -> IO String
readInput filename = readFile =<< getDataFileName ("input/" ++ filename)

readInputList :: String -> IO [String]
readInputList filename = lines <$> readInput filename

readInputParsed :: (String -> a) -> String -> IO a
readInputParsed morph filename = morph <$> readInput filename

readInputListParsed :: (String -> a) -> String -> IO [a]
readInputListParsed morph filename = fmap morph <$> readInputList filename

withInput :: String -> (String -> IO a) -> (a -> b) -> IO b
withInput filename reader f = f <$> reader filename
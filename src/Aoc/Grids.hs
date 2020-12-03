module Aoc.Grids where

import Control.Applicative (Applicative (liftA2))
import Data.List (transpose)
import Data.Vector as V (Vector, fromList, head, length, (!))

type Grid a = Vector (Vector a)

parseGrid :: [[a]] -> Grid a
parseGrid input = fromList $ [fromList line | line <- transpose input]

fromString :: String -> Grid Char
fromString = parseGrid . lines

fromStringWith :: (Char -> a) -> String -> Grid a
fromStringWith f = fmap (fmap f) . fromString

sizeX :: Grid a -> Int
sizeX = V.length

sizeY :: Grid a -> Int
sizeY = V.length . V.head

bounds :: Grid a -> (Int, Int)
bounds = liftA2 (,) sizeX sizeY

(!) :: Grid a -> (Int, Int) -> a
grid ! (x, y) = grid V.! x V.! y
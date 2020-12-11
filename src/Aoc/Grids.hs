{-# LANGUAGE DeriveTraversable #-}

module Aoc.Grids where

import Control.Applicative (Applicative (liftA2))
import Data.HashMap.Strict as M (HashMap, fromList)
import Data.List (transpose)
import Data.Vector as V (Vector, fromList, head, imap, length, map, (!), (!?))

newtype Grid a = Grid {toVector :: Vector (Vector a)} deriving (Eq, Functor, Foldable, Traversable)

parseGrid :: [[a]] -> Grid a
parseGrid input = Grid . V.fromList $ [V.fromList line | line <- transpose input]

onVector :: (Vector (Vector a) -> Vector (Vector b)) -> Grid a -> Grid b
onVector f (Grid g) = Grid $ f g

imapP :: ((Int, Int) -> a -> b) -> Grid a -> Grid b
imapP f = onVector $ imap (\x -> imap (\y -> f (x, y)))

mapP :: (a -> b) -> Grid a -> Grid b
mapP f = onVector $ V.map (V.map f)

parseGridMap :: [[a]] -> HashMap (Int, Int) a
parseGridMap input = M.fromList [((x, y), c) | (y, l) <- zip [0 ..] input, (x, c) <- zip [0 ..] l]

fromString :: String -> Grid Char
fromString = parseGrid . lines

fromStringWith :: (Char -> a) -> String -> Grid a
fromStringWith f = mapP f . fromString

sizeX :: Grid a -> Int
sizeX = V.length . toVector

sizeY :: Grid a -> Int
sizeY = V.length . V.head . toVector

bounds :: Grid a -> (Int, Int)
bounds = liftA2 (,) sizeX sizeY

inBounds :: Grid a -> (Int, Int) -> Bool
inBounds grid (x, y) =
  let (bX, bY) = bounds grid
   in (x >= 0 && y >= 0 && x < bX && y < bY)

(!) :: Grid a -> (Int, Int) -> a
grid ! (x, y) = toVector grid V.! x V.! y

(!?) :: Grid a -> (Int, Int) -> Maybe a
grid !? (x, y) = toVector grid V.!? x >>= (V.!? y)
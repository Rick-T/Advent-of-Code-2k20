{-# LANGUAGE DeriveTraversable #-}

module Aoc.Grids where

import Control.Applicative (Applicative (liftA2))
import Data.HashMap.Strict as M (HashMap, fromList)
import Data.List (transpose)
import Data.Vector as V (Vector, cons, fromList, head, imap, length, map, replicate, snoc, (!), (!?))

newtype Grid a = Grid {toVector :: Vector (Vector a)} deriving (Eq, Functor, Foldable, Traversable)

withBorder :: a -> Grid a -> Grid a
withBorder border grid =
  let sx = sizeX grid
      vs = (\v -> V.cons border $ V.snoc v border) <$> toVector grid
      topBorder = V.replicate (sx + 2) border
   in Grid $ V.cons topBorder $ V.snoc vs topBorder

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
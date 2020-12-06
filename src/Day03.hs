module Day03 where

import Aoc.Grids (Grid, bounds, fromStringWith, (!))
import Aoc.Input (readInput, withInput)
import Aoc.Util (countMatches)

data Tile = Free | Tree deriving (Eq, Show)

type Position = (Int, Int)

type Map = Grid Tile

part1 :: IO Int
part1 = withInput "Day03.txt" (readInput $ fromStringWith parseTile) $ solveSlope (3, 1)

part2 :: IO Int
part2 = withInput "Day03.txt" (readInput $ fromStringWith parseTile) solvePart2

solveSlope :: (Int, Int) -> Map -> Int
solveSlope slope grid =
  let (gridX, gridY) = bounds grid
      positions = takeWhile ((< gridY) . snd) $ iterate (step gridX slope) (0, 0)
   in countMatches (checkTree grid) positions

solvePart2 :: Map -> Int
solvePart2 grid =
  let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
   in product $ (`solveSlope` grid) <$> slopes

checkTree :: Map -> Position -> Bool
checkTree grid pos = grid ! pos == Tree

step :: Int -> (Int, Int) -> Position -> Position
step gridX (slopeX, slopeY) (x, y) = ((x + slopeX) `mod` gridX, y + slopeY)

parseTile :: Char -> Tile
parseTile '.' = Free
parseTile '#' = Tree
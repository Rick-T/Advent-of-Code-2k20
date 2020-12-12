module Day03 where

import Aoc.Grid (Grid, fromStringWith, fromTuple, inBounds, sizeX, (!))
import Aoc.Input (readInput, withInput)
import Aoc.Util (countMatches)
import Aoc.Vector (V2, mapX, origin)

data Tile = Free | Tree deriving (Eq, Show)

type Position = V2 Int

type Map = Grid Tile

-- >>> part1
-- 167
part1 :: IO Int
part1 = withInput "Day03.txt" (readInput $ fromStringWith parseTile) $ solveSlope $ fromTuple (3, 1)

-- >>> part2
-- 736527114
part2 :: IO Int
part2 = withInput "Day03.txt" (readInput $ fromStringWith parseTile) solvePart2

solveSlope :: V2 Int -> Map -> Int
solveSlope slope grid =
  let gridX = sizeX grid
      positions = takeWhile (inBounds grid) $ iterate (step gridX slope) origin
   in countMatches (checkTree grid) positions

solvePart2 :: Map -> Int
solvePart2 grid =
  let slopes = fromTuple <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
   in product $ (`solveSlope` grid) <$> slopes

checkTree :: Map -> Position -> Bool
checkTree grid pos = grid ! pos == Tree

step :: Int -> V2 Int -> Position -> Position
step gridX dr r = mapX (`mod` gridX) $ r + dr

parseTile :: Char -> Tile
parseTile '.' = Free
parseTile '#' = Tree
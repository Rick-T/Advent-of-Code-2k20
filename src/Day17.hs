module Day17 where

import Aoc.Input (readInput, withInput)
import Aoc.Parsers (parseBest)
import Aoc.Util (countMatches, iterateN)
import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Maybe (catMaybes)

type Map = HashMap Position Cube

type Position = (Int, Int, Int, Int)

data Cube = Active | Inactive deriving (Eq)

-- >>> part1
-- 257
part1 :: IO Int
part1 = withInput "Day17.txt" (readInput parse4dGridMap) solvePart1

-- >>> part2
-- 2532
part2 :: IO Int
part2 = withInput "Day17.txt" (readInput parse4dGridMap) solvePart2

solvePart1 :: Map -> Int
solvePart1 m = countMatches (== Active) $ M.elems $ iterateN 6 (step to3d) m

solvePart2 :: Map -> Int
solvePart2 m = countMatches (== Active) $ M.elems $ iterateN 6 (step id) m

step :: (Position -> Position) -> Map -> Map
step constraint m =
  let neighbours = fmap constraint $ concatMap adjacents $ M.keys m
   in foldr (update m) mempty $ S.fromList neighbours

update :: Map -> Position -> Map -> Map
update oldMap position = if findNext oldMap position == Active then M.insert position Active else id

findNext :: Map -> Position -> Cube
findNext map position =
  let neighbours = adjacentTiles map position
      activeNeighbours = countMatches (== Active) neighbours
   in case map !? position of
        Just Active -> if activeNeighbours == 3 || activeNeighbours == 2 then Active else Inactive
        _ -> if activeNeighbours == 3 then Active else Inactive

adjacentTiles :: Map -> Position -> [Cube]
adjacentTiles map pos = catMaybes $ (`M.lookup` map) <$> adjacents pos

adjacents :: Position -> [Position]
adjacents r = [add r dr | dr <- directions]

to3d :: Position -> Position
to3d (x, y, z, w) = (x, y, z, 0)

add :: Position -> Position -> Position
add (x, y, z, w) (dx, dy, dz, dw) = (x + dx, y + dy, z + dz, w + dw)

directions :: [Position]
directions = [(dx, dy, dz, dw) | dx <- [-1 .. 1], dy <- [-1 .. 1], dz <- [-1 .. 1], dw <- [-1 .. 1], dz /= 0 || dy /= 0 || dx /= 0 || dw /= 0]

parse4dGridMap :: String -> HashMap Position Cube
parse4dGridMap input = M.fromList [((x, y, 0, 0), toCube c) | (y, l) <- zip [0 ..] (lines input), (x, c) <- zip [0 ..] l]

toCube :: Char -> Cube
toCube '.' = Inactive
toCube '#' = Active
module Day11 where

import Aoc.Grids (Grid, fromStringWith, imapP, withBorder, (!))
import Aoc.Input (readInput, withInput)
import Aoc.Util (countMatches, firstEqual, hasAtLeast)
import Data.Maybe (catMaybes)

type Map = Grid Tile

data Tile = Border | Floor | Seat | Person deriving (Eq)

type StateFunction = Map -> (Int, Int) -> Tile

type TilesFunction = Map -> (Int, Int) -> [Tile]

type Position = (Int, Int)

type Direction = (Int, Int)

-- >>> part1
-- 2361
part1 :: IO Int
part1 = withInput "Day11.txt" (readInput parseMap) solvePart1

-- >>> part2
-- 2119
part2 :: IO Int
part2 = withInput "Day11.txt" (readInput parseMap) solvePart2

solvePart1 :: Map -> Int
solvePart1 = countMatches (== Person) . firstEqual . iterate (step $ nextState adjacentTiles 4)

solvePart2 :: Map -> Int
solvePart2 = countMatches (== Person) . firstEqual . iterate (step $ nextState visibleTiles 5)

step :: StateFunction -> Map -> Map
step f m = imapP (\k _ -> f m k) m

nextState :: TilesFunction -> Int -> StateFunction
nextState f n m p =
  let tile = m ! p
      others = f m p
   in case tile of
        Seat -> if Person `elem` others then Seat else Person
        Person -> if hasAtLeast n (== Person) others then Seat else Person
        Floor -> Floor
        Border -> Border

visibleTiles :: TilesFunction
visibleTiles m p = catMaybes [visibleTile m d p | d <- directions]

visibleTile :: Map -> Direction -> Position -> Maybe Tile
visibleTile m d p =
  let rayStep = add p d
   in case m ! rayStep of
        Floor -> visibleTile m d rayStep
        Person -> Just Person
        Seat -> Just Seat
        Border -> Nothing

adjacentTiles :: TilesFunction
adjacentTiles m p =
  let adjs = adjacents p
   in (m !) <$> adjs

adjacents :: Position -> [Position]
adjacents (x, y) = [(x + dx, y + dy) | (dx, dy) <- directions]

directions :: [Position]
directions = [(dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dy /= 0 || dx /= 0]

add :: Position -> Direction -> Position
add (x, y) (dx, dy) = (x + dx, y + dy)

parseMap :: String -> Map
parseMap = withBorder Border . fromStringWith parseTile

parseTile :: Char -> Tile
parseTile '#' = Person
parseTile 'L' = Seat
parseTile '.' = Floor
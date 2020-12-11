module Day11 where

import Aoc.Grids (Grid, fromStringWith, imapP, (!), (!?))
import Aoc.Input (readInput, withInput)
import Aoc.Util (countMatches, hasAtLeast)
import Data.Maybe (catMaybes)

type Map = Grid Tile

data Tile = Floor | Seat | Person deriving (Eq)

type StateFunction = Map -> (Int, Int) -> Tile

type TilesFunction = Map -> (Int, Int) -> [Tile]

part1 :: IO Int
part1 = withInput "Day11.txt" (readInput parseMap) solvePart1

part2 :: IO Int
part2 = withInput "Day11.txt" (readInput parseMap) solvePart2

solvePart1 :: Map -> Int
solvePart1 = countMatches (== Person) . firstEqual . iterate (step $ nextState adjacentTiles 4)

solvePart2 :: Map -> Int
solvePart2 = countMatches (== Person) . firstEqual . iterate (step $ nextState visibleTiles 5)

firstEqual :: Eq x => [x] -> x
firstEqual [] = undefined
firstEqual [x] = x
firstEqual (x : y : xs) = if x == y then x else firstEqual (y : xs)

step :: StateFunction -> Map -> Map
step f m = imapP (\k _ -> f m k) m

nextState :: TilesFunction -> Int -> StateFunction
nextState f n m p =
  let tile = m ! p
      adjs = f m p
   in case tile of
        Floor -> Floor
        Seat -> if Person `elem` adjs then Seat else Person
        Person -> if hasAtLeast n (== Person) adjs then Seat else Person

visibleTiles :: Map -> (Int, Int) -> [Tile]
visibleTiles m p = catMaybes [visibleTile m d p | d <- directions]

visibleTile :: Map -> (Int, Int) -> (Int, Int) -> Maybe Tile
visibleTile m d p =
  let rayStep = add d p
   in case m !? add d p of
        Nothing -> Nothing
        Just Seat -> Just Seat
        Just Person -> Just Person
        Just Floor -> visibleTile m d rayStep

adjacentTiles :: Map -> (Int, Int) -> [Tile]
adjacentTiles m p =
  let adjs = adjacents p
   in catMaybes $ (m !?) <$> adjs

adjacents :: (Int, Int) -> [(Int, Int)]
adjacents (x, y) = [(x + dx, y + dy) | (dx, dy) <- directions]

directions :: [(Int, Int)]
directions = [(dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dy /= 0 || dx /= 0]

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x, y) (dx, dy) = (x + dx, y + dy)

parseMap :: String -> Map
parseMap = fromStringWith parseTile

parseTile :: Char -> Tile
parseTile '#' = Person
parseTile 'L' = Seat
parseTile '.' = Floor
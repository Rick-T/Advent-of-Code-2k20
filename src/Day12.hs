module Day12 where

import Aoc.Input (readInputList, withInput)
import Aoc.Parsers (Parser, integer, parseBest)
import Aoc.Util (iterateN)
import Aoc.Vector
  ( V2 (V2),
    manhattanDistance,
    rot270,
    rot90,
  )
import Text.Megaparsec.Char (letterChar)

type Interpreter = (V2 Int -> Ship -> Ship)

type Turn = (V2 Int -> V2 Int)

data Instruction = Step (V2 Int) | Turn Turn | Go Int

data Ship = Ship {position :: V2 Int, waypoint :: V2 Int}

-- >>> part1
-- 796
part1 :: IO Int
part1 = withInput "Day12.txt" (readInputList $ parseBest instruction) solvePart1

-- >>> part2
-- 39446
part2 :: IO Int
part2 = withInput "Day12.txt" (readInputList $ parseBest instruction) solvePart2

solvePart1 :: [Instruction] -> Int
solvePart1 = manhattanDistance . position . moveAll stepShip (Ship 0 (V2 1 0))

solvePart2 :: [Instruction] -> Int
solvePart2 = manhattanDistance . position . moveAll stepWaypoint (Ship 0 (V2 10 1))

moveAll :: Interpreter -> Ship -> [Instruction] -> Ship
moveAll step = foldl (flip $ execute step)

execute :: Interpreter -> Instruction -> Ship -> Ship
execute _ (Turn t) = turn t
execute _ (Go i) = iterateN i forward
execute step (Step s) = step s

stepShip :: Interpreter
stepShip d (Ship p w) = Ship (p + d) w

stepWaypoint :: Interpreter
stepWaypoint d (Ship p w) = Ship p (w + d)

turn :: Turn -> Ship -> Ship
turn t (Ship p w) = Ship p (t w)

forward :: Ship -> Ship
forward (Ship p w) = Ship (p + w) w

instruction :: Parser Instruction
instruction = do
  c <- letterChar
  i <- integer
  case c of
    'N' -> return $ Step $ V2 0 i
    'S' -> return $ Step $ V2 0 (- i)
    'E' -> return $ Step $ V2 i 0
    'W' -> return $ Step $ V2 (- i) 0
    'L' -> return $ Turn $ iterateN (i `div` 90) rot90
    'R' -> return $ Turn $ iterateN (i `div` 90) rot270
    'F' -> return $ Go i
    _ -> fail "Invalid direction"
module Day12 where

import Aoc.Input (readInputList, withInput)
import Aoc.Parsers (Parser, integer, parseBest)
import Aoc.Util (iterateN)
import Text.Megaparsec.Char (letterChar)

part1 :: IO Int
part1 = withInput "Day12.txt" (readInputList $ parseBest instruction) solvePart1

part2 :: IO Int
part2 = withInput "Day12.txt" (readInputList $ parseBest instruction) solvePart2

type Position = (Int, Int)

type Interpreter = (Direction -> Ship -> Ship)

data Direction = N | S | E | W

data Turn = L | R

data InstructionType = Step Direction | Turn Turn | Go

data Instruction = Instruction InstructionType Int

data Ship = Ship {position :: Position, waypoint :: Position}

solvePart1 :: [Instruction] -> Int
solvePart1 = manhattanDistance . position . moveAll stepShip (Ship (0, 0) (1, 0))

solvePart2 :: [Instruction] -> Int
solvePart2 = manhattanDistance . position . moveAll stepWaypoint (Ship (0, 0) (10, 1))

manhattanDistance :: Position -> Int
manhattanDistance (x, y) = abs x + abs y

rotateLeft :: Position -> Position
rotateLeft (x, y) = (- y, x)

rotateRight :: Position -> Position
rotateRight (x, y) = (y, - x)

moveAll :: Interpreter -> Ship -> [Instruction] -> Ship
moveAll step = foldl (flip $ move step)

move :: Interpreter -> Instruction -> Ship -> Ship
move step (Instruction i num) = iterateN num (execute step i)

execute :: Interpreter -> InstructionType -> Ship -> Ship
execute _ (Turn t) = turn t
execute _ Go = forward
execute step (Step s) = step s

stepShip :: Interpreter
stepShip d (Ship p w) = Ship (add p d) w

stepWaypoint :: Interpreter
stepWaypoint d (Ship p w) = Ship p (add w d)

turn :: Turn -> Ship -> Ship
turn t (Ship p w) = case t of
  L -> Ship p (rotateLeft w)
  R -> Ship p (rotateRight w)

forward :: Ship -> Ship
forward (Ship (x, y) (dx, dy)) = Ship (x + dx, y + dy) (dx, dy)

add :: Position -> Direction -> Position
add (x, y) N = (x, y + 1)
add (x, y) S = (x, y - 1)
add (x, y) E = (x + 1, y)
add (x, y) W = (x - 1, y)

instruction :: Parser Instruction
instruction = do
  c <- letterChar
  i <- integer
  case c of
    'N' -> return $ Instruction (Step N) i
    'S' -> return $ Instruction (Step S) i
    'E' -> return $ Instruction (Step E) i
    'W' -> return $ Instruction (Step W) i
    'L' -> return $ Instruction (Turn L) (i `div` 90)
    'R' -> return $ Instruction (Turn R) (i `div` 90)
    'F' -> return $ Instruction Go i
    _ -> fail "Invalid direction"
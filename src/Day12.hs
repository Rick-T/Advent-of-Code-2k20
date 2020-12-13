module Day12 where

import Aoc.Input (readInputList, withInput)
import Aoc.Parsers (Parser, integer, parseBest)
import Aoc.Vector
  ( DiscreteRotation,
    V2 (V2),
    east,
    fromAngle,
    north,
    rotDiscrete,
    south,
    taxiNorm,
    west,
    (.*),
  )
import Text.Megaparsec.Char (letterChar)

type Interpreter = (V2 Int -> Ship -> Ship)

type Turn = DiscreteRotation

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
solvePart1 = taxiNorm . position . moveAll stepShip (Ship 0 (V2 1 0))

solvePart2 :: [Instruction] -> Int
solvePart2 = taxiNorm . position . moveAll stepWaypoint (Ship 0 (V2 10 1))

moveAll :: Interpreter -> Ship -> [Instruction] -> Ship
moveAll step = foldl (flip $ execute step)

execute :: Interpreter -> Instruction -> Ship -> Ship
execute _ (Turn t) (Ship p w) = Ship p (rotDiscrete t w)
execute _ (Go i) (Ship p w) = Ship (p + i .* w) w
execute step (Step s) ship = step s ship

stepShip :: Interpreter
stepShip d (Ship p w) = Ship (p + d) w

stepWaypoint :: Interpreter
stepWaypoint d (Ship p w) = Ship p (w + d)

instruction :: Parser Instruction
instruction = do
  c <- letterChar
  i <- integer
  case c of
    'N' -> return $ Step $ i .* north
    'S' -> return $ Step $ i .* south
    'E' -> return $ Step $ i .* east
    'W' -> return $ Step $ i .* west
    'L' -> Turn <$> fromAngle i
    'R' -> Turn <$> fromAngle (- i)
    'F' -> return $ Go i
    _ -> fail "Invalid direction"
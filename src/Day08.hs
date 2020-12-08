module Day08 where

import Aoc.Input (readInput, withInput)
import Aoc.Parsers (Parser, integer, parseBest)
import Control.Monad.State
  ( MonadState (get, put),
    State,
    evalState,
    gets,
    modify,
  )
import Data.IntMap as M (IntMap, fromList, insert, keys, size, (!))
import Data.IntSet as S (IntSet, insert, member)
import Data.Maybe (mapMaybe)
import Text.Megaparsec (sepBy, some)
import Text.Megaparsec.Char (letterChar, newline, spaceChar)

data Instruction = Acc Int | Jmp Int | Nop Int deriving (Show)

type Program = IntMap Instruction

data Computer = Computer {instr :: Int, acc :: Int, executed :: IntSet}

type ComputerState = State Computer

type ProgramResult = Maybe Int

data StepResult = Continue | Stop ProgramResult

-- >>> part1
-- 1744
part1 :: IO Int
part1 = withInput "Day08.txt" (readInput $ parseBest program) solvePart1

-- >>> part2
-- Just 1174
part2 :: IO (Maybe Int)
part2 = withInput "Day08.txt" (readInput $ parseBest program) solvePart2

solvePart1 :: Program -> Int
solvePart1 p = evalState (runProgram p *> gets acc) $ Computer 0 0 mempty

solvePart2 :: Program -> Maybe Int
solvePart2 p = evalState (runPrograms (fixProgram p)) $ Computer 0 0 mempty

fixProgram :: Program -> [Program]
fixProgram p = mapMaybe (modifyAt p) $ M.keys p

modifyAt :: Program -> Int -> Maybe Program
modifyAt p i = case p M.! i of
  Nop v -> Just $ M.insert i (Jmp v) p
  Acc _ -> Nothing
  Jmp v -> Just $ M.insert i (Nop v) p

runProgram :: Program -> ComputerState ProgramResult
runProgram p = do
  result <- stepProgram p
  case result of
    Stop r -> return r
    Continue -> runProgram p

runPrograms :: [Program] -> ComputerState ProgramResult
runPrograms [] = return Nothing
runPrograms (p : ps) = do
  result <- runProgram p
  case result of
    Just i -> return $ Just i
    Nothing -> reset *> runPrograms ps

stepProgram :: Program -> ComputerState StepResult
stepProgram p = do
  c <- get
  let result = checkStep c p
  case result of
    Continue -> Continue <$ nextExecution p
    _ -> return result

nextExecution :: Program -> ComputerState ()
nextExecution p = do
  i <- gets instr
  updateExecuted i <* case p M.! i of
    Nop _ -> increaseInstr 1
    Acc v -> increaseAcc v *> increaseInstr 1
    Jmp v -> increaseInstr v

checkStep :: Computer -> Program -> StepResult
checkStep (Computer i a e) p
  | i == M.size p = Stop (Just a)
  | i > M.size p || i `S.member` e = Stop Nothing
  | otherwise = Continue

reset :: ComputerState ()
reset = put $ Computer 0 0 mempty

increaseInstr :: Int -> ComputerState ()
increaseInstr v = modify $ \s -> s {instr = instr s + v}

increaseAcc :: Int -> ComputerState ()
increaseAcc v = modify $ \s -> s {acc = acc s + v}

updateExecuted :: Int -> ComputerState ()
updateExecuted i = modify $ \s -> s {executed = S.insert i $ executed s}

instruction :: Parser Instruction
instruction = opCode <* spaceChar <*> integer

opCode :: Parser (Int -> Instruction)
opCode = do
  code <- some letterChar
  case code of
    "nop" -> return Nop
    "acc" -> return Acc
    "jmp" -> return Jmp
    _ -> fail "Invalid opCode"

program :: Parser Program
program = fromList . zip [0 ..] <$> instruction `sepBy` newline
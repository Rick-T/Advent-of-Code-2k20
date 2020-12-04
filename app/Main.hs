module Main where

import Control.Monad (forM_)
import Day01 (part1, part2)
import Day02 (part1, part2)
import Day03 (part1, part2)
import Day04 (part1, part2)
import System.Environment (getArgs)
import System.TimeIt (timeItNamed)

main :: IO ()
main = do
  puzzles <- fmap read <$> getArgs
  forM_ puzzles runPuzzle

runPuzzle :: Int -> IO ()
runPuzzle p = do
  let (part1, part2) = getFuncs p
  runFirst p part1
  runSecond p part2

getFuncs :: Int -> (IO (), IO ())
getFuncs 1 = printable (Day01.part1, Day01.part2)
getFuncs 2 = printable (Day02.part1, Day02.part2)
getFuncs 3 = printable (Day03.part1, Day03.part2)
getFuncs 4 = printable (Day04.part1, Day04.part2)

printable :: (Show a, Show b) => (IO a, IO b) -> (IO (), IO ())
printable (a, b) = (print =<< a, print =<< b)

runFirst :: Int -> IO () -> IO ()
runFirst p = timeItNamed $ "Puzzle " ++ show p ++ " part 1"

runSecond :: Int -> IO () -> IO ()
runSecond p = timeItNamed $ "Puzzle " ++ show p ++ " part 2"

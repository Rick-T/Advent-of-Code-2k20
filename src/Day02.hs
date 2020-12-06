module Day02 where

import Aoc.Input (readInputList, withInput)
import Aoc.Parsers (Parser, parseBest, positiveInt)
import Aoc.Util (countMatches)
import Text.Megaparsec (some)
import Text.Megaparsec.Char (char, letterChar)

data Policy = Policy Int Int Char deriving (Show)

data DbEntry = DbEntry Policy Password deriving (Show)

type Password = String

part1 :: IO Int
part1 = withInput "Day02.txt" (readInputList parseDbEntry) solvePart1

part2 :: IO Int
part2 = withInput "Day02.txt" (readInputList parseDbEntry) solvePart2

solvePart1 :: [DbEntry] -> Int
solvePart1 = countMatches checkDbEntry

solvePart2 :: [DbEntry] -> Int
solvePart2 = countMatches checkDbEntry'

checkDbEntry :: DbEntry -> Bool
checkDbEntry (DbEntry (Policy minCount maxCount letter) password) =
  let letterCount = occurences letter password
   in letterCount >= minCount && letterCount <= maxCount

checkDbEntry' :: DbEntry -> Bool
checkDbEntry' (DbEntry (Policy pos1 pos2 letter) password) =
  let first = password !! (pos1 - 1)
      second = password !! (pos2 - 1)
   in (first == letter) /= (second == letter)

occurences :: Char -> Password -> Int
occurences letter = countMatches (== letter)

parseDbEntry :: String -> DbEntry
parseDbEntry = parseBest dbEntryParser

dbEntryParser :: Parser DbEntry
dbEntryParser = do
  minCount <- positiveInt
  _ <- char '-'
  maxCount <- positiveInt
  _ <- char ' '
  letter <- letterChar
  _ <- char ':' *> char ' '
  password <- some letterChar
  return $ DbEntry (Policy minCount maxCount letter) password

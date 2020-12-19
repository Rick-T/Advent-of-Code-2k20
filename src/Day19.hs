module Day19 where

import Aoc.Input (readInput, withInput)
import Aoc.Parsers (Parser, integer, parseBest)
import Aoc.Regex (Regex (Branch, Chain, Unit), matches)
import Control.Applicative (Alternative (some, (<|>)), optional)
import Control.Applicative.Combinators
  ( between,
    optional,
    sepBy,
    sepEndBy,
    some,
    (<|>),
  )
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Semiring as SR (foldMapP, foldMapT, sum)
import Text.Megaparsec.Char (char, letterChar, newline, string)

data Rule = Leaf Char | Tree (Regex Int)

type Rules = IntMap Rule

data Input = Input Rules [String]

-- >>> part1
-- 132
part1 :: IO Int
part1 = withInput "Day19.txt" (readInput $ parseBest inputP) solvePart1

-- >>> part2
-- 306
part2 :: IO Int
part2 = withInput "Day19.txt" (readInput $ parseBest inputP) solvePart2

solvePart1 :: Input -> Int
solvePart1 (Input map ls) =
  let regex = construct 0 map
   in length $ filter (matches regex) ls

solvePart2 :: Input -> Int
solvePart2 (Input map ls) =
  let regex = construct 0 (update map)
   in length $ filter (matches regex) ls

update :: Rules -> Rules
update rules = foldr (uncurry M.insert) rules $ parseBest ruleP <$> ["8: 42 | 42 8", "11: 42 31 | 42 11 31"]

construct :: Int -> Rules -> Regex Char
construct i map = go (map M.! i)
  where
    go (Leaf c) = Unit c
    go (Tree t) = fromTree t
    fromTree (Unit c) = go $ map M.! c
    fromTree (Branch a b) = Branch (fromTree a) (fromTree b)
    fromTree (Chain a b) = Chain (fromTree a) (fromTree b)

inputP :: Parser Input
inputP = Input <$> (mapP <* newline) <*> messages

messages :: Parser [String]
messages = some (char 'a' <|> char 'b') `sepBy` newline

mapP :: Parser Rules
mapP = M.fromList <$> ruleP `sepEndBy` newline

ruleP :: Parser (Int, Rule)
ruleP = (,) <$> (integer <* string ": ") <*> (letterRule <|> numberRule)

letterRule :: Parser Rule
letterRule = Leaf <$> between (char '"') (char '"') letterChar

numberRule :: Parser Rule
numberRule = Tree . SR.sum <$> chainNumbers `sepBy` char '|'

chainNumbers :: Parser (Regex Int)
chainNumbers = optional (char ' ') *> (foldMapT Unit <$> integer `sepEndBy` char ' ')
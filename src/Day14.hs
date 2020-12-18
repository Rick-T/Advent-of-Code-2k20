module Day14 where

import Aoc.Input (readInput, withInput)
import Aoc.Parsers (Parser, integer, parseBest)
import Aoc.Util (fromBinary, toBinary)
import Control.Applicative (Alternative (some, (<|>)))
import Data.Functor (($>))
import Data.IntMap (IntMap, union)
import Data.IntMap.Strict (elems, insert)
import Text.Megaparsec (between, sepBy, try)
import Text.Megaparsec.Char (char, newline, string)

data MaskBit = X | One | Zero deriving (Show, Eq)

type Mask = [MaskBit]

type Address = (Int, Int)

data InputLine = Mask Mask | Memory Address

type Input = [InputLine]

type Accumulator = Mask -> Address -> IntMap Int

type Interpreter = Mask -> Int -> [Int]

-- >>> part1
-- 10717676595607
part1 :: IO Int
part1 = withInput "Day14.txt" (readInput $ parseBest input) solvePart1

-- >>> part2
-- 3974538275659
part2 :: IO Int
part2 = withInput "Day14.txt" (readInput $ parseBest input) solvePart2

solvePart1 :: Input -> Int
solvePart1 = sum . elems . applyAll (maskValues direct)

solvePart2 :: Input -> Int
solvePart2 = sum . elems . applyAll (maskAddrs floating)

applyAll :: Accumulator -> Input -> IntMap Int
applyAll accumulator = go (repeat X) mempty
  where
    go _ map [] = map
    go _ map (Mask mask : ls) = go mask map ls
    go mask map (Memory l : ls) = go mask (accumulator mask l `union` map) ls

maskValues :: Interpreter -> Accumulator
maskValues interpreter mask (add, val) = foldr (insert add) mempty (interpreter mask val)

maskAddrs :: Interpreter -> Accumulator
maskAddrs interpreter mask (add, val) = foldr (`insert` val) mempty (interpreter mask add)

direct :: Interpreter
direct m val = return $ fromBinary $ zipWith applyMask m (toBinary val)
  where
    applyMask Zero = const '0'
    applyMask One = const '1'
    applyMask X = id

floating :: Interpreter
floating m val = fromBinary <$> go m (toBinary val) [[]]
  where
    go [] _ l = l
    go _ [] l = l
    go (Zero : m) (x : xs) l = go m xs $ (x :) <$> l
    go (One : m) (_ : xs) l = go m xs $ ('1' :) <$> l
    go (X : m) (_ : xs) ls = go m xs $ [x : l | x <- ['0', '1'], l <- ls]

input :: Parser Input
input = inputLine `sepBy` newline

inputLine :: Parser InputLine
inputLine = try (Memory <$> address) <|> Mask <$> mask

address :: Parser Address
address = (,) <$> between (string "mem[") (string "] = ") integer <*> integer

mask :: Parser Mask
mask = string "mask = " *> some maskbit

maskbit :: Parser MaskBit
maskbit =
  char '1' $> One
    <|> char '0' $> Zero
    <|> char 'X' $> X
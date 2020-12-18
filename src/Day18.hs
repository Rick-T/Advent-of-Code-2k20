module Day18 where

import Aoc.Input (readInputList, withInput)
import Aoc.Parsers (Parser, integer, parseBest)
import Control.Applicative (Alternative ((<|>)))
import Control.Applicative.Combinators (between)
import Control.Monad.Combinators.Expr
  ( Operator (InfixL),
    makeExprParser,
  )
import Data.Functor (($>))
import Text.Megaparsec.Char (char, string)

data Expr = Number Int | Plus Expr Expr | Times Expr Expr

-- >>> part1
-- 3159145843816
part1 :: IO Int
part1 = withInput "Day18.txt" (readInputList $ parseBest $ expr [[plus, times]]) (sum . fmap eval)

-- >>> part2
-- 55699621957369
part2 :: IO Int
part2 = withInput "Day18.txt" (readInputList $ parseBest $ expr [[plus], [times]]) (sum . fmap eval)

eval :: Expr -> Int
eval (Number i) = i
eval (Plus i j) = eval i + eval j
eval (Times i j) = eval i * eval j

expr :: [[Operator Parser Expr]] -> Parser Expr
expr ops =
  let term = (Number <$> integer) <|> between (char '(') (char ')') (expr ops)
   in makeExprParser term ops

plus :: Operator Parser Expr
plus = InfixL $ string " + " $> Plus

times :: Operator Parser Expr
times = InfixL $ string " * " $> Times
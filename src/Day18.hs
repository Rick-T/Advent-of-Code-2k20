module Day18 where

import Aoc.Input (readInputList, withInput)
import Aoc.Parsers (Parser, integer, parseBest)
import Control.Applicative (Alternative ((<|>)))
import Control.Applicative.Combinators (between)
import Data.Functor (($>))
import Text.Megaparsec (MonadParsec (try))
import Text.Megaparsec.Char (char, string)

data Expr = Number Int | Plus Expr Expr | Times Expr Expr

part1 :: IO Int
part1 = withInput "Day18.txt" (readInputList $ parseBest parsePart1) sumEval

part2 :: IO Int
part2 = withInput "Day18.txt" (readInputList $ parseBest parsePart2) sumEval

sumEval :: [Expr] -> Int
sumEval = sum . fmap eval

parsePart1 :: Parser Expr
parsePart1 = expr [plus <|> times]

parsePart2 :: Parser Expr
parsePart2 = expr [times, plus]

eval :: Expr -> Int
eval (Number i) = i
eval (Plus i j) = eval i + eval j
eval (Times i j) = eval i * eval j

expr :: [Parser (Expr -> Expr -> Expr)] -> Parser Expr
expr ops =
  let term = (Number <$> integer) <|> between (char '(') (char ')') (expr ops)
   in foldr reduceOp term ops

reduceOp :: Parser (Expr -> Expr -> Expr) -> Parser Expr -> Parser Expr
reduceOp op term = try (chain term op term) <|> term

chain :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr -> Parser Expr
chain start op next = do
  x <- start
  f <- op
  y <- next
  let result = return $ f x y
  chain result op next <|> result

plus :: Parser (Expr -> Expr -> Expr)
plus = string " + " $> Plus

times :: Parser (Expr -> Expr -> Expr)
times = string " * " $> Times
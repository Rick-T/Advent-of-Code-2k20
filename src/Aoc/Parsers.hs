module Aoc.Parsers where

import Control.Monad (liftM2)
import Data.Functor (($>))
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec (Parsec, option, parseMaybe, some)
import Text.Megaparsec.Char (char, digitChar)

type Parser = Parsec Void String

parseBest :: Parser a -> String -> a
parseBest parser = fromJust . parseMaybe parser

integer :: (Integral i, Read i) => Parser i
integer = liftM2 (*) signed positiveInt

signed :: Integral i => Parser i
signed = option 1 (char '-' $> (-1))

positiveInt :: (Integral i, Read i) => Parser i
positiveInt = read <$> some digitChar
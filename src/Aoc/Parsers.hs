module Aoc.Parsers where

import Control.Monad (liftM2)
import Data.Char (isDigit)
import Data.Functor (($>))
import Text.ParserCombinators.ReadP (ReadP, char, eof, many1, option, readP_to_S, satisfy)

parseBest :: ReadP a -> String -> a
parseBest parser = fst . head . readP_to_S (parser <* eof)

integer :: (Integral i, Read i) => ReadP i
integer = liftM2 (*) signed positiveInt

signed :: Integral i => ReadP i
signed = option 1 (char '-' $> (-1))

positiveInt :: (Integral i, Read i) => ReadP i
positiveInt = read <$> many1 (satisfy isDigit)
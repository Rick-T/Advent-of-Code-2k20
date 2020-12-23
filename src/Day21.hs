module Day21 where

import Aoc.Input (readInput, readInputList, readPuzzleInput, withInput)
import Aoc.Parsers (Parser, parseBest)
import Control.Applicative.Combinators (sepBy, sepEndBy, some)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as S
import Data.List (intercalate, sort)
import Text.Megaparsec.Char (char, letterChar, newline, string)

type Allergen = String

type Ingredient = String

type Lookup = HashMap Allergen (HashSet Ingredient)

type Input = [(HashSet Ingredient, [Allergen])]

-- >>> part1
-- 2461
part1 :: IO Int
part1 = withInput "Day21.txt" (readInputList $ parseBest foodP) solvePart1

-- >>> part2
-- "ltbj,nrfmm,pvhcsn,jxbnb,chpdjkf,jtqt,zzkq,jqnhd"
part2 :: IO String
part2 = withInput "Day21.txt" (readInputList $ parseBest foodP) solvePart2

solvePart1 :: Input -> Int
solvePart1 input =
  let safe = safeIngredients input
   in length [ingredient | recipe <- fst <$> input, ingredient <- S.toList recipe, ingredient `S.member` safe]

solvePart2 :: Input -> String
solvePart2 = intercalate "," . fmap snd . sort . M.toList . reduce . buildLookup

reduce :: Lookup -> HashMap Allergen Ingredient
reduce = go mempty
  where
    go final uncertain
      | M.null uncertain = final
      | otherwise =
        let finalized = M.map (head . S.toList) $ M.filter ((== 1) . S.size) uncertain
            dropFinalized s = foldr S.delete s (M.elems finalized)
            newFinal = M.union final finalized
            newUncertain = dropFinalized <$> M.difference uncertain finalized
         in go newFinal newUncertain

safeIngredients :: Input -> HashSet String
safeIngredients input =
  let allIngredients = S.unions $ fst <$> input
      lookup = buildLookup input
      unsafeIngredients = S.unions $ M.elems lookup
   in S.difference allIngredients unsafeIngredients

buildLookup :: Input -> Lookup
buildLookup = go mempty
  where
    go map [] = map
    go map ((ingredients, allergens) : rest) =
      let
       in go (foldr (\a -> M.insertWith S.intersection a ingredients) map allergens) rest

foodP :: Parser (HashSet Ingredient, [Allergen])
foodP = do
  ingredients <- some letterChar `sepEndBy` char ' '
  _ <- string "(contains "
  allergens <- some letterChar `sepBy` string ", "
  _ <- char ')'
  return (S.fromList ingredients, allergens)
module Day23 where

import Control.Monad.ST (ST, runST)
import Data.Char (intToDigit)
import Data.Foldable (foldlM)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as VM

part1 :: Int
part1 = read $ V.toList $ V.map intToDigit $ elemsAfter 1 $ playGame 100 input

part2 :: Int
part2 = V.product $ V.take 2 $ elemsAfter 1 $ playGame 10000000 (input ++ [10 .. 1000000])

elemsAfter :: Int -> Vector Int -> Vector Int
elemsAfter i v = V.init $ V.postscanl' (\last -> const $ v V.! last) i $ V.tail v

playGame :: Int -> [Int] -> Vector Int
playGame iterations values = runST $ do
  let assocs = zip values (tail $ cycle values)
  let limit = length values
  vector <- VM.new $ limit + 1
  mapM_ (uncurry $ VM.write vector) assocs
  foldlM
    (const . move vector)
    (head values)
    [1 .. iterations]
  V.freeze vector

move :: MVector s Int -> Int -> ST s Int
move vector current = do
  first <- VM.read vector current
  second <- VM.read vector first
  third <- VM.read vector second
  next <- VM.read vector third
  let dest = destination (VM.length vector) current [first, second, third]
  afterDest <- VM.read vector dest
  VM.write vector current next
  VM.write vector dest first
  VM.write vector third afterDest
  return next

destination :: Int -> Int -> [Int] -> Int
destination limit current taken
  | current == 1 = destination limit limit taken
  | current -1 `elem` taken = destination limit (current - 1) taken
  | otherwise = current - 1

input :: [Int]
input = [4, 8, 7, 9, 1, 2, 3, 6, 5]
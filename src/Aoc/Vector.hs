{-# LANGUAGE DeriveTraversable #-}

module Aoc.Vector where

import Control.Applicative (Applicative (liftA2))

data V2 a = V2 a a deriving (Functor, Foldable, Traversable)

instance Applicative V2 where
  pure a = V2 a a
  V2 f g <*> V2 x y = V2 (f x) (g y)

instance Num a => Num (V2 a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  negate = fmap negate
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Semigroup a => Semigroup (V2 a) where
  V2 x y <> V2 dx dy = V2 (x <> dx) (y <> dy)

instance Monoid a => Monoid (V2 a) where
  mempty = V2 mempty mempty

len2 :: Num a => V2 a -> a
len2 (V2 x y) = x * x + y * y

len :: Floating a => V2 a -> a
len (V2 x y) = sqrt (x * x + y * y)

rotate :: Floating a => a -> V2 a -> V2 a
rotate i (V2 x y) =
  let x' = x * cos i - y * sin i
      y' = y * sin i + x * cos i
   in V2 x' y'

rotateDeg :: Floating a => a -> V2 a -> V2 a
rotateDeg i = rotate (deg2rad i)

flipX :: Num a => V2 a -> V2 a
flipX (V2 x y) = V2 (- x) y

flipY :: Num a => V2 a -> V2 a
flipY (V2 x y) = V2 x (- y)

rot90 :: Num a => V2 a -> V2 a
rot90 (V2 x y) = V2 (- y) x

rot180 :: Num a => V2 a -> V2 a
rot180 (V2 x y) = V2 (- x) (- y)

rot270 :: Num a => V2 a -> V2 a
rot270 (V2 x y) = V2 y (- x)

deg2rad :: Floating a => a -> a
deg2rad d = pi * d / 180

manhattanDistance :: Num a => V2 a -> a
manhattanDistance (V2 x y) = abs x + abs y
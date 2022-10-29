{-# LANGUAGE FlexibleInstances #-}

module Phi(
  Phi, phi, phiInverse, phiSquared, baseValue,
  phiDouble, toDouble
) where

import Linear.Epsilon (Epsilon)
import FieldExtensions

data Phi

instance Root Phi where
  rootProduct (AlgExt a b) (AlgExt c d) = AlgExt (prod0 + prod2) (prod1 + prod2) where
                    prod0 = a * c
                    prod1 = a * d + b * c
                    prod2 = b * d
  rootInverse v = conjugate v * baseValue prodInverse where
                       prodInverse =  1 / baseComponent (v * conjugate v)
  rootString = "phi"

instance (Ord a, Epsilon a) => Ord (AlgExt Phi a) where -- why does this require Eq (AlgExt Phi a)?
  (AlgExt xb xp) <= (AlgExt yb yp) = let
    a = (yb - xb)
    b = (yp - xp)
    discr = -a * a - a * b + b * b
    in (a >= 0 && b >= 0) || (a < 0 && b > 0 && discr > 0) || (a > 0 && b < 0 && discr < 0)

instance (Eq a) => Eq (AlgExt Phi a) where
  x == y = (baseComponent x == baseComponent y) && (extComponent x == extComponent y)

baseValue :: (Num a) => a -> AlgExt Phi a
baseValue x = AlgExt x 0

phi :: (Num a) => AlgExt Phi a
phi = AlgExt 0 1

phiSquared :: (Num a) => AlgExt Phi a
phiSquared = AlgExt 1 1

phiInverse :: (Num a) => AlgExt Phi a
phiInverse = AlgExt (-1) 1

-- value times conjugate value is real
conjugate :: (Num a) => AlgExt Phi a -> AlgExt Phi a
conjugate (AlgExt x y) = baseValue x - phiInverse * baseValue y

phiDouble :: Double
phiDouble = (1 + sqrt 5) / 2

toDouble :: AlgExt Phi Rational -> Double
toDouble (AlgExt x y) = fromRational x + fromRational y * phiDouble
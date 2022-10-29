module Sqrt2(Sqrt2) where

import FieldExtensions

data Sqrt2

instance Root Sqrt2 where
  rootProduct (AlgExt a b) (AlgExt c d) = AlgExt (a * c + 2 * b * d) (b * c + a * d)
  rootInverse (AlgExt a b) = AlgExt (a / denom) (-b / denom) where
    denom = a * a - 2 * b * b
  rootString = "Sqrt2"
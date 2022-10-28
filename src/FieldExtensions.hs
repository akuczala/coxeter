{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FieldExtensions (
  AlgExt(AlgExt, baseComponent, extComponent),
  Root(rootInverse, rootProduct)
) where

import Control.Applicative (liftA2)
import Linear.Epsilon (Epsilon (nearZero))

class Root r where
  rootInverse :: (Fractional a) => AlgExt r a -> AlgExt r a
  rootProduct :: (Num a) => AlgExt r a -> AlgExt r a -> AlgExt r a
  
data AlgExt r a = AlgExt {baseComponent :: a, extComponent :: a} deriving (Show)

instance (Root r, Epsilon a) => Epsilon (AlgExt r a) where
  nearZero (AlgExt x y) = nearZero x && nearZero y

instance Functor (AlgExt r) where
  fmap f x = AlgExt (f $ baseComponent x) (f $ extComponent x)
  
instance Applicative (AlgExt r) where
  pure x = AlgExt x x
  f <*> y = AlgExt{baseComponent = baseComponent f (baseComponent y), extComponent= extComponent f (extComponent y)}

instance (Root r, Num a) => Num (AlgExt r a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  x * y = rootProduct x y
  fromInteger x = AlgExt (fromInteger x) 0
  abs (AlgExt a b) = AlgExt (a * a + b * b) 0 -- there exists a nice absolute value but i don't know it
  signum x = x

instance (Root r, Fractional a) => Fractional (AlgExt r a) where
  fromRational x = AlgExt (fromRational x) 0
  recip x = rootInverse x

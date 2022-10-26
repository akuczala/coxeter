module FieldExtensionPhi(
  PhiExt(PhiExt), phi, phiInverse, phiSquared, baseValue,
  phiDouble, toDouble
) where

import Control.Applicative (liftA2)
import Linear.Epsilon (Epsilon (nearZero))
import PrettyPrint

data PhiExt a = PhiExt {baseComponent :: a, extComponent :: a} deriving (Show)

instance (Epsilon a) => Epsilon (PhiExt a) where
  nearZero (PhiExt x y) = nearZero x && nearZero y

instance (PrettyPrint a, Num a, Eq a) => PrettyPrint (PhiExt a) where
  prettyPrint (PhiExt x 0) = prettyPrint x
  prettyPrint (PhiExt 0 1) = "Phi"
  prettyPrint (PhiExt 0 y) = prettyPrint y ++ "Phi"
  prettyPrint (PhiExt x 1) = "(" ++ prettyPrint x ++ " + " ++ "Phi" ++ ")"
  prettyPrint (PhiExt x y) = "(" ++ prettyPrint x ++ " + " ++ prettyPrint y ++ "Phi" ++ ")"

instance Functor PhiExt where
  fmap f x = PhiExt (f $ baseComponent x) (f $ extComponent x)
  
instance Applicative PhiExt where
  pure x = PhiExt x x
  -- liftA2 f x y = Root2Ext{baseComponent = f (baseComponent x) (baseComponent y), extComponent= f (extComponent x) (extComponent y)}
  f <*> y = PhiExt{baseComponent = baseComponent f (baseComponent y), extComponent= extComponent f (extComponent y)}

instance (Num a) => Num (PhiExt a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (PhiExt a b) * (PhiExt c d) = PhiExt (prod0 + prod2) (prod1 + prod2) where
    prod0 = a * c
    prod1 = a * d + b * c
    prod2 = b * d
  fromInteger x = PhiExt (fromInteger x) 0
  abs (PhiExt a b) = PhiExt (a * a + b * b) 0 -- there exists a nice absolute value but i don't know it
  signum x = x

baseValue :: (Num a) => a -> PhiExt a
baseValue x = PhiExt x 0

phi :: (Num a) => PhiExt a
phi = PhiExt 0 1

phiSquared :: (Num a) => PhiExt a
phiSquared = PhiExt 1 1

phiInverse :: (Num a) => PhiExt a
phiInverse = PhiExt (-1) 1

-- value times conjugate value is real
conjugate :: (Num a) => PhiExt a -> PhiExt a
conjugate (PhiExt x y) = baseValue x - phiInverse * baseValue y

instance (Fractional a) => Fractional (PhiExt a) where
  fromRational x = baseValue (fromRational x)
  recip v = conjugate v * baseValue prodInverse where
     prodInverse =  1 / baseComponent (v * conjugate v)
     
phiDouble :: Double
phiDouble = (1 + sqrt 5) / 2

toDouble :: PhiExt Rational -> Double
toDouble (PhiExt x y) = fromRational x + fromRational y * phiDouble
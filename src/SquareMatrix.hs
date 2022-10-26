{-# LANGUAGE UndecidableInstances #-}

module SquareMatrix(
  SquareMatrix(SquareMatrix, getSquareMatrix),
) where

import Linear.Vector
import Linear.Epsilon (Epsilon(nearZero))
import PrettyPrint (PrettyPrint(prettyPrint))
import Linear.Matrix ((!*!), identity)
import VectorExtras(prettyPrintVector)
  
newtype SquareMatrix m a = SquareMatrix {getSquareMatrix :: m (m a)}

instance (Show (m (m a))) => Show (SquareMatrix m a) where
  show (SquareMatrix x) = show x

instance (Functor m, Foldable m, PrettyPrint a) => PrettyPrint (SquareMatrix m a) where
  prettyPrint (SquareMatrix x) = foldl1 (++) (fmap (\v -> prettyPrintVector v ++ "\n") x)
  
instance (Epsilon (m (m a)), Epsilon a) => Eq (SquareMatrix m a) where
  (SquareMatrix x) == (SquareMatrix y) = nearZero $ x - y

instance (Applicative m, Traversable m, Additive m, Num a) => Semigroup (SquareMatrix m a) where
  (SquareMatrix x) <> (SquareMatrix y) = SquareMatrix $ x !*! y

instance (Applicative m, Traversable m, Additive m, Num a) => Monoid (SquareMatrix m a) where
  mappend = (<>)
  mempty = SquareMatrix identity
  
--instance (Eq (m (m a))) => Eq (SquareMatrix m a) where
--  (SquareMatrix x) == (SquareMatrix y) = x == y

instance (Num (m(m a))) => Num (SquareMatrix m a) where
  (SquareMatrix x) + (SquareMatrix y) = SquareMatrix $ x + y
  (SquareMatrix x) - (SquareMatrix y) = SquareMatrix $ x - y
  (SquareMatrix x) * (SquareMatrix y) = SquareMatrix $ x * y
  abs (SquareMatrix x) = SquareMatrix $ abs x
  signum (SquareMatrix x) = SquareMatrix $ signum x
  fromInteger x = SquareMatrix $ fromInteger x

instance (Foldable m, Applicative m, Additive m, Epsilon (m (m a)), Epsilon a, Floating a) => Ord (SquareMatrix m a) where
  (SquareMatrix x) <= (SquareMatrix y) = or $ or <$> liftI2 (liftI2 (\x y -> nearZero $ x - y)) x y
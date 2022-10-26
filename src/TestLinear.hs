{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module TestLinear where

import Linear.V3
import Linear.Metric
import Linear.Matrix
import Linear.Vector
import Linear.Epsilon (nearZero, Epsilon)
import Data.List (nub)
import FieldExtensions (Root2Ext (..))
import FieldExtensionPhi(PhiExt(..), phi, phiInverse, baseValue)

import Data.Ratio (numerator, denominator)
import PrettyPrint (PrettyPrint(prettyPrint))

matrixPowLame :: (Applicative m, Traversable m, Additive m, Num a) => m (m a) -> Int -> m (m a)
matrixPowLame m n = foldl (!*!) identity [m | _<-[1..n]]

monoidPow :: (Monoid m) => m -> Int -> m
monoidPow m n = mconcat [m | _<-[1..n]]

monoidOrder :: (Eq m, Monoid m) => Int -> m -> Int
monoidOrder maxOrder m = 1 + length ls where
   ls = takeWhile (\n -> monoidPow m n /= mempty) [1..maxOrder]

newtype SquareMatrix m a = SquareMatrix {getSquareMatrix :: m (m a)}
instance (Applicative m, Traversable m, Additive m, Num a) => Semigroup (SquareMatrix m a) where
  (SquareMatrix x) <> (SquareMatrix y) = SquareMatrix $ x !*! y
instance (Applicative m, Traversable m, Additive m, Num a) => Monoid (SquareMatrix m a) where
  mappend = (<>)
  mempty = SquareMatrix identity
  
instance (Epsilon (m (m a)), Epsilon a) => Eq (SquareMatrix m a) where
  (SquareMatrix x) == (SquareMatrix y) = nearZero $ x - y
  
--instance (Eq (m (m a))) => Eq (SquareMatrix m a) where
--  (SquareMatrix x) == (SquareMatrix y) = x == y

instance (Num (m(m a))) => Num (SquareMatrix m a) where
  (SquareMatrix x) + (SquareMatrix y) = SquareMatrix $ x + y
  (SquareMatrix x) - (SquareMatrix y) = SquareMatrix $ x - y
  (SquareMatrix x) * (SquareMatrix y) = SquareMatrix $ x * y
  abs (SquareMatrix x) = SquareMatrix $ abs x
  signum (SquareMatrix x) = SquareMatrix $ signum x
  fromInteger x = SquareMatrix $ fromInteger x

prettyPrintVector :: (Foldable m, Functor m, PrettyPrint a) => m a -> String
prettyPrintVector v = foldl1 (\x y -> x ++ " " ++ y) $ fmap prettyPrint v

instance (Show (m (m a))) => Show (SquareMatrix m a) where
  show (SquareMatrix x) = show x
instance (Functor m, Foldable m, PrettyPrint a) => PrettyPrint (SquareMatrix m a) where
  prettyPrint (SquareMatrix x) = foldl1 (++) (fmap (\x -> prettyPrintVector x ++ "\n") x)


--instance (Foldable m, Applicative m, Additive m, Epsilon (m (m a)), Epsilon a, Floating a) => Ord (SquareMatrix m a) where
--  (SquareMatrix x) <= (SquareMatrix y) = or $ or <$> liftI2 (liftI2 (\x y -> nearZero $ x - y)) x y
   
matrixPow :: (Applicative m, Traversable m, Additive m, Num a) => m (m a) -> Int -> m (m a)
matrixPow m n = getSquareMatrix $ monoidPow (SquareMatrix m) n

matrixOrder :: (Epsilon (m (m a)), Applicative m, Traversable m, Additive m, Epsilon a) => Int -> m (m a) -> Int
matrixOrder maxOrder m = monoidOrder maxOrder (SquareMatrix m)

matrixOrder' :: (Epsilon (m (m a)), Applicative m, Traversable m, Additive m, Epsilon a) => m (m a) -> Int
matrixOrder' = matrixOrder 50

orderTable :: (Epsilon (m (m a)), Applicative m, Traversable m, Additive m, Epsilon a) => [m (m a)] -> [[Int]]
orderTable gs = [[matrixOrder' (g0 !*! g1) | g1 <- gs] | g0 <- gs]

getAngle :: (Metric v, Floating a) => v a -> v a -> a
getAngle v0 v1 = acos $ dot v0 v1 / (norm v0 * norm v0)

reflectAlong :: (Metric v, Floating a) => v a -> v a -> v a
reflectAlong u v = vPerp ^-^ vPara where
  vPara = project (signorm u) v
  vPerp = v ^-^ vPara
  
reflectAlongUnit :: (Metric v, Num a) => v a -> v a -> v a
reflectAlongUnit u v = vPerp ^-^ vPara where
  vPara = dot u v *^ u
  vPerp = v ^-^ vPara

reflectMatrix :: (Metric v, Traversable v, Applicative v, Num a) => v a -> v (v a)
reflectMatrix u = fmap (reflectAlongUnit u) identity

cleanEpsilon :: (Epsilon a) => a -> a
cleanEpsilon x = if nearZero x then 0 else x

uniqueListProduct :: (Eq a, Semigroup a) => [a] -> [a] -> [a]
uniqueListProduct ls1 ls2 = nub [x1 <> x2 | x1 <- ls1, x2 <- ls2]

fromGenerators' :: (Eq a, Monoid a) => Int -> [a] -> [a] -> [a]
fromGenerators' 0 gens ls = ls
fromGenerators' maxIter gens ls =
  if length newList == length ls
    then newList
    else fromGenerators' (maxIter - 1) gens newList where
      newList = uniqueListProduct gens ls 
      
fromGenerators :: (Eq a, Monoid a) => Int -> [a] -> [a]
fromGenerators maxIter gens = fromGenerators' maxIter gens gens

instance Epsilon Rational where
  nearZero x = x == 0

type VectorField = PhiExt Rational
-- vs :: [V3 Double]
--vs = [
--  unit _x,
--  V3 (cos $ pi / 3) (sin $ pi / 3) 0,
--  V3 0 a (sqrt $ 1 - a * a)
--     ] where
--        a = (1 + sqrt 5) / (2 * sqrt 3)

--vs = [
--  unit _x,
--  V3 (sqrt 2 / 2) (sqrt 2 / 2) 0,
--  V3 0 (1 / sqrt 2) (1 / sqrt 2)
--     ] :: [V3 Double]


--root2 :: VectorField
--root2 = Root2Ext 0 1
--
--vs = [
--  unit _x,
--  V3 (root2 / 2) (root2 /2) 0,
--  V3 0 (1 / root2) (1 / root2)
--     ] :: [V3 VectorField]

phiDouble :: Double
phiDouble = (1 + sqrt 5) / 2
toDouble :: PhiExt Rational -> Double
toDouble (PhiExt x y) = fromRational x + fromRational y * phiDouble

w = V3 (baseValue 0) phi phiInverse :: V3 VectorField
vs = [
  (V3 1 1 1 - w) ^* (phi / 2),
  (V3 1 1 (-1) - w) / 2,
  (V3 (-phi) phiInverse 0 - w) / 2
  ] :: [V3 VectorField]

rs = map reflectMatrix vs :: [V3 (V3 VectorField)]

gens = identity : rs :: [V3 (V3 VectorField)]

groupElements = map (fmap cleanEpsilon . getSquareMatrix) $ fromGenerators 100 (map SquareMatrix gens)
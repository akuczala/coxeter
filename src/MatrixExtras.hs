module MatrixExtras(
  matrixPow,
  matrixOrder,
  matrixOrder',
  cleanEpsilon,
  orderTable
) where

import Linear.Epsilon (Epsilon(nearZero))
import Linear.Vector (Additive)
import Linear.Matrix

import SquareMatrix(SquareMatrix(SquareMatrix, getSquareMatrix))
import MonoidExtras(monoidPow, monoidOrder)


matrixPow :: (Applicative m, Traversable m, Additive m, Num a) => m (m a) -> Int -> m (m a)
matrixPow m n = getSquareMatrix $ monoidPow (SquareMatrix m) n

matrixOrder :: (Epsilon (m (m a)), Applicative m, Traversable m, Additive m, Epsilon a) => Int -> m (m a) -> Int
matrixOrder maxOrder m = monoidOrder maxOrder (SquareMatrix m)

matrixOrder' :: (Epsilon (m (m a)), Applicative m, Traversable m, Additive m, Epsilon a) => m (m a) -> Int
matrixOrder' = matrixOrder 50

cleanEpsilon :: (Epsilon a) => a -> a
cleanEpsilon x = if nearZero x then 0 else x
  
orderTable :: (Epsilon (m (m a)), Applicative m, Traversable m, Additive m, Epsilon a) => Int -> [m (m a)] -> [[Int]]
orderTable maxOrder gs = [[matrixOrder maxOrder (g0 !*! g1) | g1 <- gs] | g0 <- gs]
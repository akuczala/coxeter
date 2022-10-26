module MonoidExtras(
  monoidPow,
  monoidOrder,
  uniqueListProduct,
  fromGenerators,
  orderTable
  ) where

import Data.List (nub)


monoidPow :: (Monoid m) => m -> Int -> m
monoidPow m n = mconcat [m | _<-[1..n]]

monoidOrder :: (Eq m, Monoid m) => Int -> m -> Int
monoidOrder maxOrder m = 1 + length ls where
   ls = takeWhile (\n -> monoidPow m n /= mempty) [1..maxOrder]

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

orderTable :: (Eq a, Monoid a) => Int -> [a] -> [[Int]]
orderTable maxOrder gs = [[monoidOrder maxOrder (g0 <> g1) | g1 <- gs] | g0 <- gs]
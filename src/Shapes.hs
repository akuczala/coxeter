module Shapes(
  transformFace,
  generateShape,
  Face(Face),
  getFaces
) where

import Linear.Matrix
import Linear.Vector
import Linear.V3
import Data.List (sort, nub)
import VectorExtras (prettyPrintVector)
import PrettyPrint

newtype Face p = Face [p] deriving Show
instance Functor Face where
  fmap f (Face points) = Face $ map f points

instance (Ord a, Eq a) => Eq (Face a) where
  (Face x) == (Face y) = sort x == sort y

newtype Shape p = Shape {getFaces :: [Face p]}

transformFace :: (Foldable v, Additive v, Num a) => v (v a) -> Face (v a) -> Face (v a)
transformFace g (Face points) = Face $ map (g !*) points

generateShape :: (Foldable v, Additive v, Ord (v a), Num a) => [v (v a)] -> Face (v a) -> Shape (v a)
generateShape groupElements face = Shape $ nub $ map (\g -> transformFace g face) groupElements
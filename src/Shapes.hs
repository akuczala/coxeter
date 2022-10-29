module Shapes(
  transformFace,
  generateShape,
  Face(Face, getFacePoints),
  Shape(Shape),
  getFaces,
  pentagon,
  dodecahedron
) where

import Linear.Matrix
import Linear.Vector
import Linear.V3
import Data.List (sort, nub)
import Groups
import MatrixExtras
import Linear (Epsilon)

newtype Face p = Face {getFacePoints :: [p]} deriving Show
instance Functor Face where
  fmap f (Face points) = Face $ map f points

instance (Ord a) => Eq (Face a) where
  (Face x) == (Face y) = sort x == sort y

newtype Shape p = Shape {getFaces :: [Face p]}

transformFace :: (Foldable v, Additive v, Num a) => v (v a) -> Face (v a) -> Face (v a)
transformFace g (Face points) = Face $ map (g !*) points

generateShape :: (Foldable v, Additive v, Ord (v a), Num a) => [v (v a)] -> Face (v a) -> Shape (v a)
generateShape groupElements face = Shape $ nub $ map (\g -> transformFace g face) groupElements

pentagon :: (Fractional a) => a -> Face (V3 a)
pentagon phi = let
  gens = (getGenerators . h2Basis) phi
  g5 = matrixPow (head gens !*! (gens !! 1)) 3
  in Face $ map (\i -> matrixPow g5 i !* pure 1) [0..4]
  
dodecahedron :: (Ord a, Epsilon a, Fractional a) => a -> Shape (V3 a)
dodecahedron phi = generateShape (h2Elements phi) (pentagon phi)
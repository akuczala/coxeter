module Shapes(
  transformFace,
  generateShape,
  Face(Face, getFacePoints),
  Shape(Shape),
  getFaces,
  pentagon,
  dodecahedron,
  triangle,
  icosahedron
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
  g5 = matrixPow 3 (head gens !*! (gens !! 1))
  in Face $ map (\i -> matrixPow i g5 !* pure 1) [0..4]
  
dodecahedron :: (Ord a, Epsilon a, Fractional a) => a -> Shape (V3 a)
dodecahedron phi = generateShape (h2Elements phi) (pentagon phi)

triangle :: (Epsilon a, Fractional a) => a -> Face (V3 a)
triangle phi = let
  g3 = h2Elements phi !! 93
  v0 = V3 (1/phi) 1 0
  in Face $ map (\i -> matrixPow i g3 !* v0) [0..2]
  
icosahedron :: (Ord a, Epsilon a, Fractional a) => a -> Shape (V3 a)
icosahedron phi = generateShape (h2Elements phi) (triangle phi)
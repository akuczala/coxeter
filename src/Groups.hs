module Groups(
  b3Basis,
  h2Basis,
  getGenerators,
  generateGroupElements,
  h2Generators,
  h2Elements
) where

import Linear.Vector
import Linear.Metric
import Linear.V3
import Reflections (reflectMatrix)
import Linear (Epsilon, identity)
import MatrixExtras (cleanEpsilon)
import SquareMatrix
import MonoidExtras


b3Basis :: (Fractional a) => a -> [V3 a]
b3Basis sqrt2 = [
  unit _x,
  V3 (sqrt2 / 2) (sqrt2 / 2) 0,
  V3 0 (1 / sqrt2) (1 / sqrt2)
     ]

h2Basis :: (Fractional a) => a -> [V3 a]
h2Basis phi = let
  phiInverse = 1 / phi
  w = V3 0 phi phiInverse in
    [
    (V3 1 1 1 - w) ^* (phi / 2),
    (V3 1 1 (-1) - w) / 2,
    (V3 (-phi) phiInverse 0 - w) / 2
    ]

getGenerators :: (Applicative v, Traversable v, Metric v, Fractional a) => [v a] -> [v (v a)]
getGenerators = map reflectMatrix

generateGroupElements :: (Applicative v, Traversable v, Metric v, Epsilon (v a), Epsilon (v (v a)), Epsilon a) => Int -> [v (v a)] -> [v (v a)]
generateGroupElements maxIter generators = map (fmap cleanEpsilon . getSquareMatrix) $ fromGenerators maxIter (map SquareMatrix $ identity : generators)

h2Generators :: (Fractional a) => a -> [V3 (V3 a)]
h2Generators phi = getGenerators $ h2Basis phi

h2Elements :: (Epsilon a, Fractional a) => a -> [V3 (V3 a)]
h2Elements phi = generateGroupElements 100 $ h2Generators phi
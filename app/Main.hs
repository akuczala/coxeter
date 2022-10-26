module Main (main) where

import Linear.V3
import Linear.Vector

import FieldExtensionPhi(PhiExt, phi, phiInverse, baseValue)
import Linear.Matrix (identity)
import Reflections(reflectMatrix)
import SquareMatrix
import MonoidExtras
import MatrixExtras(cleanEpsilon)
import Orphans

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

w = V3 (baseValue 0) phi phiInverse :: V3 VectorField
vs = [
  (V3 1 1 1 - w) ^* (phi / 2),
  (V3 1 1 (-1) - w) / 2,
  (V3 (-phi) phiInverse 0 - w) / 2
  ] :: [V3 VectorField]

rs = map reflectMatrix vs :: [V3 (V3 VectorField)]

gens = identity : rs :: [V3 (V3 VectorField)]

groupElements = map (fmap cleanEpsilon . getSquareMatrix) $ fromGenerators 100 (map SquareMatrix gens)

main :: IO ()
main = do
  print (length groupElements)
  return ()

module Main (main) where

import Linear.V3
import Linear.Vector

import FieldExtensionPhi(PhiExt(PhiExt), phi, phiInverse, baseValue, toDouble)
import Linear.Matrix ((!*), (!*!))
import Reflections(reflectMatrix)
import SquareMatrix
import MonoidExtras
import MatrixExtras(matrixPow, matrixOrder')
import Orphans
import VectorExtras (prettyPrintVector)
import Data.List (nub, sort, sortBy, nubBy)
import Data.Ratio (numerator)
import Groups
import Shapes
import PrettyPrint (prettyPrint)

type VectorField = PhiExt Rational

gens = (getGenerators . h2Basis) phi  :: [V3 (V3 VectorField)]

groupElements = generateGroupElements 100 gens

ones = pure 1 :: V3 VectorField

g5 = matrixPow ((gens !! 0) !*! (gens !! 1)) 3

pentagon = Face $ map (\i -> matrixPow g5 i !* ones) [0..4] :: Face (V3 VectorField)

dodeca = generateShape groupElements pentagon

vectorToList :: V3 a -> [a]
vectorToList = foldl (\xs x -> x:xs) [] 

main :: IO ()
main = do
  print $ orderTable (length groupElements) (map SquareMatrix gens)
  print (length groupElements)
  print $ length $ getFaces dodeca
  print $ nub $ map matrixOrder' groupElements
  -- print $ fmap (fmap (vectorToList . fmap (phiExtToList . fmap numerator))) faces
  -- print $ fmap (fmap (vectorToList . fmap toDouble)) faces
  print (let
    vecFun = vectorToList . fmap toDouble;
    faceFun = fmap vecFun
    in faceFun <$> getFaces dodeca
    )
  print (fmap (vectorToList . fmap prettyPrint) <$> getFaces dodeca)
  return ()

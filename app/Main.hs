module Main (main) where

import Linear.V3
import SquareMatrix
import MonoidExtras
import MatrixExtras(matrixOrder')
import Groups
import Shapes
import PrettyPrint (prettyPrint)
import FieldExtensionPhi
import Data.List (nub)
import Orphans()

phiValue :: PhiExt Rational
phiValue = phi

vectorToList :: V3 a -> [a]
vectorToList = foldl (\xs x -> x:xs) [] 

groupElements :: [V3 (V3 (PhiExt Rational))]
groupElements = h2Elements phiValue

dodeca :: Shape (V3 (PhiExt Rational))
dodeca = dodecahedron phiValue

main :: IO ()
main = do
  print $ orderTable (length groupElements) (map SquareMatrix (h2Generators phiValue))
  print (length groupElements)
  print $ length $ getFaces (dodecahedron phiValue)
  print $ nub $ map matrixOrder' groupElements
  print (let
    vecFun = vectorToList . fmap toDouble;
    faceFun = fmap vecFun
    in faceFun <$> getFaces dodeca
    )
  print (fmap (vectorToList . fmap prettyPrint) <$> getFaces dodeca)
  return ()

module Main (main) where

import Linear.V3
import MatrixExtras
import Groups
import Shapes
import PrettyPrint (prettyPrint)
import Phi
import Sqrt2
import Data.List (nub)
import Orphans()
import FieldExtensions

sqrt2Value :: AlgExt Sqrt2 Rational
sqrt2Value = AlgExt 0 1

phiValue :: AlgExt Phi Rational
phiValue = phi

vectorToList :: V3 a -> [a]
vectorToList = foldl (\xs x -> x:xs) [] 

groupElements :: [V3 (V3 (AlgExt Phi Rational))]
groupElements = h2Elements phiValue

dodeca :: Shape (V3 (AlgExt Phi Rational))
dodeca = dodecahedron phiValue

icosa :: Shape (V3 (AlgExt Phi Rational))
icosa = icosahedron phiValue

main :: IO ()
main = do
  print $ length (b3Elements sqrt2Value)
  print $ orderTable (length groupElements) (h2Generators phiValue)
  print (length groupElements)
  print $ length $ getFaces dodeca
  print $ length $ getFaces icosa
  print $ nub $ map matrixOrder' groupElements
--  print (let
--    vecFun = vectorToList . fmap toDouble;
--    faceFun = fmap vecFun
--    in faceFun <$> getFaces dodeca
--    )
  print $ prettyPrint (getFacePoints . fmap vectorToList <$> getFaces dodeca)
  print $ prettyPrint (getFacePoints . fmap vectorToList <$> getFaces icosa)
  return ()

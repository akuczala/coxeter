module VectorExtras(
  prettyPrintVector
) where

import PrettyPrint (PrettyPrint(prettyPrint))
import Linear.Metric (Metric, dot, norm)


prettyPrintVector :: (Foldable m, Functor m, PrettyPrint a) => m a -> String
prettyPrintVector v = foldl1 (\x y -> x ++ " " ++ y) $ fmap prettyPrint v

getAngle :: (Metric v, Floating a) => v a -> v a -> a
getAngle v0 v1 = acos $ dot v0 v1 / (norm v0 * norm v0)
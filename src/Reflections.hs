module Reflections where

import Linear.Metric
import Linear.Vector
import Linear.Matrix


reflectAlong :: (Metric v, Floating a) => v a -> v a -> v a
reflectAlong u v = vPerp ^-^ vPara where
  vPara = project (signorm u) v
  vPerp = v ^-^ vPara
  
reflectAlongUnit :: (Metric v, Num a) => v a -> v a -> v a
reflectAlongUnit u v = vPerp ^-^ vPara where
  vPara = dot u v *^ u
  vPerp = v ^-^ vPara

reflectMatrix :: (Metric v, Traversable v, Applicative v, Num a) => v a -> v (v a)
reflectMatrix u = fmap (reflectAlongUnit u) identity
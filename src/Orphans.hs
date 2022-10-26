{-# LANGUAGE FlexibleInstances #-}

module Orphans where

import Linear.Epsilon (Epsilon(nearZero))


instance Epsilon Rational where
  nearZero x = x == 0
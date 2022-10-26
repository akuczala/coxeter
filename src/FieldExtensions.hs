{-# LANGUAGE FlexibleInstances #-}

module FieldExtensions (Root2Ext (..), makeExt) where

import Control.Applicative (liftA2)
import Linear.Epsilon (Epsilon (nearZero))
import PrettyPrint

data Root2Ext a = Root2Ext {baseComponent :: a, extComponent :: a} deriving (Show)

instance (Epsilon a) => Epsilon (Root2Ext a) where
  nearZero (Root2Ext x y) = nearZero x && nearZero y

instance (PrettyPrint a, Num a, Eq a) => PrettyPrint (Root2Ext a) where
  prettyPrint (Root2Ext x 0) = prettyPrint x
  prettyPrint (Root2Ext 0 y) = prettyPrint y ++ "r2"
  prettyPrint (Root2Ext x y) = prettyPrint x ++ " + " ++ prettyPrint y ++ "r2"

makeExt :: a -> a -> Root2Ext a
makeExt = Root2Ext

-- zeroRoot = Root2Ext {baseComponent=0, extComponent=0}

instance Functor Root2Ext where
  fmap f x = Root2Ext (f $ baseComponent x) (f $ extComponent x)
  
instance Applicative Root2Ext where
  pure x = Root2Ext x x
  -- liftA2 f x y = Root2Ext{baseComponent = f (baseComponent x) (baseComponent y), extComponent= f (extComponent x) (extComponent y)}
  f <*> y = Root2Ext{baseComponent = baseComponent f (baseComponent y), extComponent= extComponent f (extComponent y)}

instance (Num a) => Num (Root2Ext a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (Root2Ext a b) * (Root2Ext c d) = Root2Ext (a * c + 2 * b * d) (b * c + a * d)
  fromInteger x = Root2Ext (fromInteger x) 0
  abs (Root2Ext a b) = Root2Ext (a * a + b * b) 0 -- there exists a nice absolute value but i don't know it
  signum x = x

instance (Fractional a) => Fractional (Root2Ext a) where
  fromRational x = Root2Ext (fromRational x) 0
  recip v@(Root2Ext x y) = Root2Ext (x / prod) (-y / prod) where
     prod = baseComponent (v * v) -- this is wrong
  

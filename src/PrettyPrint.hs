{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint(PrettyPrint, prettyPrint) where

import Data.Ratio (numerator, denominator)


class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint Rational where
  prettyPrint x
    | numerator x == 0 = "0"
    | denominator x == 1 = show $ numerator x
    | otherwise = show (numerator x) ++ "/" ++ show (denominator x)

instance PrettyPrint Double where
  prettyPrint x = show x

instance PrettyPrint Int where
  prettyPrint x = show x
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

instance PrettyPrint Char where
  prettyPrint x = show x
  
instance (PrettyPrint a) => PrettyPrint [a] where
  -- prettyPrint xs = foldl (\left x -> left ++ prettyPrint x ++ ", ") "[" (take (length xs - 1) xs)
  prettyPrint ls = "[" ++ pp ls ++ "]" where
    pp [] = ""
    pp [x] = prettyPrint x
    pp (x:xs) = prettyPrint x ++ ", " ++ pp xs
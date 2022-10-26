module Algebra(Expr(Literal, Variable)) where

import Data.Ratio (denominator, numerator)
import PrettyPrint (PrettyPrint(prettyPrint))

data Expr a =
  Literal a
  | Variable Char
  | Sum (Expr a) (Expr a)
  | Negate (Expr a)
  | Prod (Expr a) (Expr a)
  | Ratio (Expr a) (Expr a)
  | Abs (Expr a) deriving (Show)
  
instance (Eq a, Num a) => Num (Expr a) where
  (Literal 0) + y = y
  y + (Literal 0) = y
  x + y = Sum x y
  
  negate (Literal x) = Literal (-x)
  negate (Variable x) = Negate (Variable x)
  negate (Negate x) = x
  negate x = Negate x
  (Literal 0) * _ = Literal 0
  _ * (Literal 0) = Literal 0
  (Literal 1) * y = y
  y * (Literal 1) = y
  x * y = Prod x y
  abs x = Abs x
  signum x = Ratio x (Abs x)
  fromInteger x = Literal $ fromInteger x

instance (Eq a, Num a) => Fractional (Expr a) where
  recip x = Ratio 1 x
  fromRational x = Ratio (fromInteger $ numerator x) (fromInteger $ denominator x)
  
parenthesize :: String -> String
parenthesize x = "(" ++ x ++ ")"

instance (PrettyPrint a) => PrettyPrint (Expr a) where
  prettyPrint (Literal x) = prettyPrint x
  prettyPrint (Variable x) = [x]
  prettyPrint (Negate x) = "-" ++ prettyPrint x
  prettyPrint (Sum x y) = parenthesize $ prettyPrint x ++ " + " ++ prettyPrint y
  prettyPrint (Prod x y) = prettyPrint x ++ prettyPrint y
  prettyPrint (Ratio x y) = prettyPrint x ++ "/" ++ prettyPrint y
  prettyPrint (Abs x) = "|" ++ prettyPrint x ++ "|"
  
instance (Eq a) => Eq (Expr a) where
  (Literal x) == (Literal y) = x == y
  (Variable x) == (Variable y) = x == y
  (Sum x y) == (Sum w z) = (x == w) && (z == y)
  (Negate x) == (Negate y) = x == y
  (Prod x y) == (Prod w z) = (x == w) && (z == y)
  (Ratio x y) == (Ratio w z) = (x == w) && (z == y)
  (Abs x) == (Abs y) = x == y
  _ == _ = False
module Intermediate.Specs.Lexicals.Operator where

data IUnaryOp
  = Not
  | Invert
  | Negate
  | PreIncrement
  | PostIncrement
  | PreDecrement
  | PostDecrement
  deriving (Eq, Show, Ord)

data IBinaryOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Xor
  | BoolAnd
  | BoolOr
  | Equal
  | Neq
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | LShift
  | RShift
  | Dot
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
  | ModAssign
  | AndAssign
  | OrAssign
  | XorAssign
  | LShiftAssign
  | RShiftAssign
  | Index
  deriving (Eq, Show, Ord)

module Intermediate.Specs.Expression where

import Data.Word

data IUnaryOp
  = Not
  | Invert
  | Negate
  | PreIncrement
  | PostIncrement
  | PreDecrement
  | PostDecrement
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data IExpr
  = BoolLiteral Bool
  | IntLiteral {isHex :: Bool, intVal :: Integer}
  | BytesLiteral [Word8]
  | UnaryExpr {unaryOp :: IUnaryOp, uExpr :: IExpr}
  | BinaryExpr {binaryOp :: IBinaryOp, lExpr :: IExpr, rExpr :: IExpr}
  | TernaryExpr {ternaryCond :: IExpr, ternaryTrueBranch :: IExpr, ternaryFalseBranch :: IExpr}
  deriving (Eq, Show)
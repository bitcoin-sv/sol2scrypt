module Intermediate.Specs.Expression where

import Intermediate.Specs.Lexical

data IExpr
  = LiteralExpr ILiteral
  | IdentifierExpr IIdentifier
  | UnaryExpr {unaryOp :: IUnaryOp, uExpr :: IExpr}
  | BinaryExpr {binaryOp :: IBinaryOp, lExpr :: IExpr, rExpr :: IExpr}
  | TernaryExpr {ternaryCond :: IExpr, ternaryTrueBranch :: IExpr, ternaryFalseBranch :: IExpr}
  deriving (Eq, Show, Ord)


newtype IExprList = IExprList { unExpressionList :: [IExpr] } deriving (Eq, Ord, Show)
module IR.Specs.Expression where

import IR.Specs.Lexical

data IExpr
  = LiteralExpr ILiteral
  | IdentifierExpr IIdentifier
  | Parens {enclosedExpr :: IExpr}
  | UnaryExpr {unaryOp :: IUnaryOp, uExpr :: IExpr}
  | BinaryExpr {binaryOp :: IBinaryOp, lExpr :: IExpr, rExpr :: IExpr}
  | TernaryExpr {ternaryCond :: IExpr, ternaryTrueBranch :: IExpr, ternaryFalseBranch :: IExpr}
  deriving (Eq, Show, Ord)


newtype IExprList = IExprList { unExpressionList :: [IExpr] } deriving (Eq, Ord, Show)
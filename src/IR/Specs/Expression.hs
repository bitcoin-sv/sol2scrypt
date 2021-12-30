module IR.Specs.Expression where

import IR.Specs.Lexical

data IExpression
  = LiteralExpr ILiteral
  | IdentifierExpr IIdentifier
  | ParensExpr {enclosedExpr :: IExpression}
  | UnaryExpr {unaryOp :: IUnaryOp, uExpr :: IExpression}
  | BinaryExpr {binaryOp :: IBinaryOp, lExpr :: IExpression, rExpr :: IExpression}
  | TernaryExpr {ternaryCond :: IExpression, ternaryTrueBranch :: IExpression, ternaryFalseBranch :: IExpression}
  | MemberAccess {instanceExpr :: IExpression, member :: IIdentifier}
  | FunctionCall {funcExpr :: IExpression, funcParamExprs :: [IExpression]}
  | ArrayLiteral {arrayVal :: [IExpression]}
  deriving (Eq, Show, Ord)


newtype IExprList = ExprList { unExprList :: [IExpression] } deriving (Eq, Ord, Show)
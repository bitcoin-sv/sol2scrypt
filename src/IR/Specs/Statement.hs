module IR.Specs.Statement where

import IR.Specs.Expression
import IR.Specs.Lexicals.Identifier (IIdentifier)
import IR.Specs.Variable

data IStatement 
  = IfStmt {cond :: IExpression, trueBranch :: IStatement, falseBranch :: Maybe IStatement}
  | ExprStmt IExpression
  | AssignStmt [Maybe IIdentifier] [IExpression]
  | DeclareStmt [Maybe IParam] [IExpression]
  | RequireStmt {verifyExpr :: IExpression}
  | ReturnStmt {retExpr :: IExpression}
  | BlockStmt IBlock
  | EmptyStmt
  deriving (Show, Eq, Ord)

newtype IBlock = Block [IStatement] deriving (Show, Eq, Ord)

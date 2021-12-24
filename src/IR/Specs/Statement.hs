module IR.Specs.Statement where

import IR.Specs.Expression
import IR.Specs.Lexicals.Identifier (IIdentifier)
import IR.Specs.Param (IParam)

data IStatement 
  = IfStmt {cond :: IExpr, trueBranch :: IStatement, falseBranch :: Maybe IStatement}
  | ExprStmt IExpr
  | AssignStmt [Maybe IIdentifier] [IExpr]
  | DeclareStmt [Maybe IParam] [IExpr]
  deriving (Show, Eq, Ord)

newtype IBlock = Block [IStatement] deriving (Show, Eq, Ord)

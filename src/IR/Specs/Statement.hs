module IR.Specs.Statement where

import IR.Specs.Expression
import IR.Specs.Lexicals.Identifier (IIdentifier)

data IStatement 
  = IfStmt {cond :: IExpr, trueBranch :: IStatement, falseBranch :: Maybe IStatement}
  | ExprStmt IExpr
  | AssignStmt [Maybe IIdentifier] [IExpr]
  deriving (Show, Eq, Ord)

newtype IBlock = Block [IStatement] deriving (Show, Eq, Ord)

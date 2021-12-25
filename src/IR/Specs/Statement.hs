module IR.Specs.Statement where

import IR.Specs.Expression
import IR.Specs.Lexicals.Identifier (IIdentifier)
import IR.Specs.Variable

data IStatement 
  = IfStmt {cond :: IExpr, trueBranch :: IStatement, falseBranch :: Maybe IStatement}
  | ExprStmt IExpr
  | AssignStmt [Maybe IIdentifier] [IExpr]
  | DeclareStmt [Maybe IParam] [IExpr]
  | RequireStmt {verifyExpr :: IExpr}
  | ReturnStmt {retExpr :: IExpr}
  deriving (Show, Eq, Ord)

newtype IBlock = Block [IStatement] deriving (Show, Eq, Ord)

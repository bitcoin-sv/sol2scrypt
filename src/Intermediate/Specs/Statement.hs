module Intermediate.Specs.Statement where

import Intermediate.Specs.Expression

data IStatement 
  = IfStmt {cond :: IExpr, trueBranch :: IStatement, falseBranch :: Maybe IStatement}
  | ExprStmt IExpr
  deriving (Show, Eq, Ord)

newtype IBlock = Block [IStatement] deriving (Show, Eq, Ord)

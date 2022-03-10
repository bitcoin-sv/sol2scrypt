module IR.Specs.Statement where

import IR.Specs.Expression
import IR.Specs.Variable
import IR.Specs.Lexicals.Identifier

data IStatement 
  = IfStmt {cond :: IExpression, trueBranch :: IStatement, falseBranch :: Maybe IStatement}
  | ExprStmt IExpression
  | AssignStmt [IExpression] [IExpression]
  | DeclareStmt [Maybe IParam] [IExpression]
  | RequireStmt {verifyExpr :: IExpression}
  | ReturnStmt {retExpr :: IExpression}
  | BlockStmt IBlock
  | ExitStmt IExpression
  | LoopStmt {loopCount :: IExpression, loopVar :: Maybe IIdentifier, loopBody :: IStatement}
  deriving (Show, Eq, Ord)

newtype IBlock = Block [IStatement] deriving (Show, Eq, Ord)

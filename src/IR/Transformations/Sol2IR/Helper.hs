module IR.Transformations.Sol2IR.Helper where

import Solidity

exprExist :: Expression -> Statement -> Bool
exprExist e (BlockStatement (Block stmts)) = foldr ((||) . exprExist e) False stmts
exprExist e (IfStatement cond trueBranch maybeFalseBranch) =
  e == cond
    || exprExist e trueBranch
    || case maybeFalseBranch of
      Nothing -> False
      Just fb -> exprExist e fb
exprExist e (SimpleStatementExpression e') = e == e'
exprExist e (SimpleStatementVariableDeclarationList _ rhs) = foldr ((||) . (==) e) False rhs
exprExist e (SimpleStatementVariableAssignmentList _ rhs) = foldr ((||) . (==) e) False rhs
exprExist _ _ = False
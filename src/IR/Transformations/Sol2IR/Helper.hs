module IR.Transformations.Sol2IR.Helper where

import Solidity

exprExistsInStmt :: Expression -> Statement -> Bool
exprExistsInStmt e (BlockStatement (Block stmts)) = foldr ((||) . exprExistsInStmt e) False stmts
exprExistsInStmt e (IfStatement cond trueBranch maybeFalseBranch) =
  exprExistsInExpr e cond
    || exprExistsInStmt e trueBranch
    || case maybeFalseBranch of
      Nothing -> False
      Just fb -> exprExistsInStmt e fb
exprExistsInStmt e (SimpleStatementExpression e') = exprExistsInExpr e e'
exprExistsInStmt e (SimpleStatementVariableDeclarationList _ rhs) = foldr ((||) . exprExistsInExpr e) False rhs
exprExistsInStmt e (SimpleStatementVariableAssignmentList _ rhs) = foldr ((||) . exprExistsInExpr e) False rhs
exprExistsInStmt _ _ = False

exprExistsInExpr :: Expression -> Expression -> Bool
exprExistsInExpr e e'@(Unary _ ue) = e == e' || exprExistsInExpr' e ue
exprExistsInExpr e e'@(Binary _ lhs rhs) = e == e' || exprExistsInExpr' e lhs || exprExistsInExpr' e rhs
exprExistsInExpr e e'@(Ternary _ cond lhs rhs) = e == e' || exprExistsInExpr' e cond || exprExistsInExpr' e lhs || exprExistsInExpr' e rhs
exprExistsInExpr e e'@(FunctionCallNameValueList f pl) =
   e == e' || exprExistsInExpr' e f || case pl of
    Nothing -> False
    Just (NameValueList ns) -> foldr ((||) . exprExistsInExpr e . snd) False ns
exprExistsInExpr e e'@(FunctionCallExpressionList f pl) =
   e == e' || exprExistsInExpr' e f || case pl of
    Nothing -> False
    Just (ExpressionList ns) -> foldr ((||) . exprExistsInExpr e) False ns
exprExistsInExpr e e'@(MemberAccess me _) = e == e' || exprExistsInExpr' e me
exprExistsInExpr e e'@(Literal l) =
   e == e' || case l of
    PrimaryExpressionTupleExpression (RoundBrackets es) -> foldr ((||) . exprExistsInExpr e) False es
    PrimaryExpressionTupleExpression (SquareBrackets es) -> foldr ((||) . exprExistsInExpr e) False es
    _ -> False
exprExistsInExpr e e' = e == e'

exprExistsInExpr' :: Expression -> Expression -> Bool
exprExistsInExpr' decExpr ancExpr = decExpr == ancExpr || exprExistsInExpr decExpr ancExpr
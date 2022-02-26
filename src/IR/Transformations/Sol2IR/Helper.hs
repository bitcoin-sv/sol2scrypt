module IR.Transformations.Sol2IR.Helper where

import Solidity

exprExistsInStmt :: Expression SourceRange -> Statement SourceRange -> Bool
exprExistsInStmt e (BlockStatement (Block stmts _)) = foldr ((||) . exprExistsInStmt e) False stmts
exprExistsInStmt e (IfStatement cond trueBranch maybeFalseBranch _) =
  exprExistsInExpr e cond
    || exprExistsInStmt e trueBranch
    || case maybeFalseBranch of
      Nothing -> False
      Just fb -> exprExistsInStmt e fb
exprExistsInStmt e (SimpleStatementExpression e' _) = exprExistsInExpr e e'
exprExistsInStmt e (SimpleStatementVariableDeclarationList _ rhs _) = foldr ((||) . exprExistsInExpr e) False rhs
exprExistsInStmt e (SimpleStatementVariableAssignmentList _ rhs _) = foldr ((||) . exprExistsInExpr e) False rhs
exprExistsInStmt _ _ = False


exprExistsInExpr :: Expression SourceRange -> Expression SourceRange -> Bool
exprExistsInExpr e e'@(Unary _ ue _) = e == e' || exprExistsInExprEx e ue
exprExistsInExpr e e'@(Binary _ lhs rhs _) = e == e' || exprExistsInExprEx e lhs || exprExistsInExprEx e rhs
exprExistsInExpr e e'@(Ternary _ cond lhs rhs _) = e == e' || exprExistsInExprEx e cond || exprExistsInExprEx e lhs || exprExistsInExprEx e rhs
exprExistsInExpr e e'@(FunctionCallNameValueList f pl _) =
   e == e' || exprExistsInExprEx e f || case pl of
    Nothing -> False
    Just (NameValueList ns) -> foldr ((||) . exprExistsInExpr e . snd) False ns
exprExistsInExpr e e'@(FunctionCallExpressionList f pl _) =
   e == e' || exprExistsInExprEx e f || case pl of
    Nothing -> False
    Just (ExpressionList ns) -> foldr ((||) . exprExistsInExpr e) False ns
exprExistsInExpr e e'@(MemberAccess me _ _) = e == e' || exprExistsInExprEx e me
exprExistsInExpr e e'@(Literal l) =
   e == e' || case l of
    PrimaryExpressionTupleExpression (RoundBrackets es _) -> foldr ((||) . exprExistsInExpr e) False es
    PrimaryExpressionTupleExpression (SquareBrackets es _) -> foldr ((||) . exprExistsInExpr e) False es
    _ -> False
exprExistsInExpr e e' = e == e'


exprExistsInExprEx :: Expression SourceRange -> Expression SourceRange -> Bool
exprExistsInExprEx decExpr ancExpr = decExpr == ancExpr || exprExistsInExpr decExpr ancExpr
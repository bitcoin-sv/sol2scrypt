
module IR.Transformations.Sol2IR.Helper where
import Text.Parsec.Pos

import Solidity



eqIdentifier :: Identifier SourceRange -> Identifier SourceRange -> Bool
eqIdentifier (Identifier iid _) (Identifier iid' _) = iid == iid'

eqStateMutability :: StateMutability SourceRange -> StateMutability SourceRange -> Bool 
eqStateMutability (StateMutability ability _) (StateMutability ability' _) = ability == ability'

compareList :: [a] -> [a] -> (a -> a -> Bool) -> Bool
compareList [] [] _ = True
compareList [] ((_:_)) _ = False
compareList ((_:_)) [] _ = False
compareList ((e:xs) ) ((e':ys)) fn = fn e e' && compareList xs ys fn
  

eqPrimaryExpression :: PrimaryExpression SourceRange -> PrimaryExpression SourceRange -> Bool
eqPrimaryExpression (PrimaryExpressionBooleanLiteral (BooleanLiteral v _)) (PrimaryExpressionBooleanLiteral (BooleanLiteral v' _)) = v == v'
eqPrimaryExpression (PrimaryExpressionNumberLiteral (NumberLiteral v _)) (PrimaryExpressionNumberLiteral (NumberLiteral v' _)) = v == v'
eqPrimaryExpression (PrimaryExpressionHexLiteral (HexLiteral v _)) (PrimaryExpressionHexLiteral (HexLiteral v' _)) = v == v'
eqPrimaryExpression (PrimaryExpressionStringLiteral (StringLiteral v _)) (PrimaryExpressionStringLiteral (StringLiteral v' _)) = v == v'
eqPrimaryExpression (PrimaryExpressionTupleExpression (RoundBrackets v _)) (PrimaryExpressionTupleExpression (RoundBrackets v' _)) 
  = compareList v v' exprEq
eqPrimaryExpression (PrimaryExpressionTupleExpression (SquareBrackets v _)) (PrimaryExpressionTupleExpression (SquareBrackets v' _)) 
  = compareList v v' exprEq
eqPrimaryExpression (PrimaryExpressionIdentifier (Identifier v _)) (PrimaryExpressionIdentifier (Identifier v' _)) 
  = v == v'
eqPrimaryExpression (PrimaryExpressionElementaryTypeNameExpression et) (PrimaryExpressionElementaryTypeNameExpression et') 
  = eqElementaryTypeName et et'
  
eqPrimaryExpression _ _ = False 


eqElementaryTypeName :: ElementaryTypeName  SourceRange -> ElementaryTypeName  SourceRange -> Bool 
eqElementaryTypeName (ElementaryTypeName t _) (ElementaryTypeName t' _) = t == t'


eqUserDefinedTypeName :: UserDefinedTypeName  SourceRange -> UserDefinedTypeName  SourceRange -> Bool
eqUserDefinedTypeName (UserDefinedTypeName []) (UserDefinedTypeName [])  = True
eqUserDefinedTypeName (UserDefinedTypeName []) (UserDefinedTypeName (_:_))  = False
eqUserDefinedTypeName (UserDefinedTypeName (_:_)) (UserDefinedTypeName [])  = False
eqUserDefinedTypeName (UserDefinedTypeName (e:xs) ) (UserDefinedTypeName (e':ys))  = eqIdentifier e e' && eqUserDefinedTypeName (UserDefinedTypeName xs) (UserDefinedTypeName ys)
  

eqTypeName :: TypeName SourceRange -> TypeName SourceRange -> Bool
eqTypeName (TypeNameMapping et tn _) (TypeNameMapping et' tn' _) = eqElementaryTypeName et et' &&  eqTypeName tn tn'
eqTypeName (TypeNameFunctionTypeName l stateMutability ml _) (TypeNameFunctionTypeName l' stateMutability' ml' _) = 
   typeNameListEq l l' && compareList stateMutability stateMutability' eqStateMutability && maybeTypeNameListEq ml ml'
eqTypeName (TypeNameElementaryTypeName et _) (TypeNameElementaryTypeName et' _) = eqElementaryTypeName et et'
eqTypeName (TypeNameUserDefinedTypeName l _) (TypeNameUserDefinedTypeName l' _) = eqUserDefinedTypeName l l'
eqTypeName (TypeNameArrayTypeName t me _) (TypeNameArrayTypeName t' me' _) = eqTypeName t t' &&  maybeExprEq me me'
eqTypeName _ _ = False 

typeNameListEq :: TypeNameList SourceRange -> TypeNameList SourceRange -> Bool
typeNameListEq (TypeNameList l) (TypeNameList l')  = compareList l l' eqTypeName
  
maybeTypeNameListEq :: Maybe (TypeNameList SourceRange) -> Maybe (TypeNameList SourceRange) -> Bool
maybeTypeNameListEq Nothing Nothing  = True
maybeTypeNameListEq Nothing (Just _) = False
maybeTypeNameListEq (Just _) Nothing = False
maybeTypeNameListEq (Just a) (Just b) = typeNameListEq a b


nameValueListEq :: NameValueList SourceRange -> NameValueList SourceRange -> Bool
nameValueListEq (NameValueList []) (NameValueList [])  = True
nameValueListEq (NameValueList []) (NameValueList (_:_))  = False
nameValueListEq (NameValueList (_:_)) (NameValueList [])  = False
nameValueListEq (NameValueList ((i,e):xs) ) (NameValueList ((i',e'):ys))  = (eqIdentifier i i' &&  exprEq e e') && nameValueListEq (NameValueList xs) (NameValueList ys)

expressionListEq :: ExpressionList SourceRange -> ExpressionList SourceRange -> Bool
expressionListEq (ExpressionList l) (ExpressionList l')  = compareList l l' exprEq


exprEq :: Expression SourceRange -> Expression SourceRange -> Bool
exprEq (Unary op1 e1 _) (Unary op2 e2 _) = op1 == op2 && exprEq e1 e2
exprEq (Binary op1 e1 e1' _) (Binary op2 e2 e2' _) = op1 == op2 && exprEq e1 e2 && exprEq e1' e2'
exprEq (Ternary op1 e1 e1' e1'' _) (Ternary op2 e2 e2' e2'' _) = op1 == op2 && exprEq e1 e2 && exprEq e1' e2' && exprEq e1'' e2''
exprEq (FunctionCallNameValueList e1 e1' _) (FunctionCallNameValueList e2 e2' _) =
   case (e1', e2') of
     (Nothing, Nothing) -> exprEq e1 e2
     (Just nameValueList1, Just nameValueList2) -> exprEq e1 e2 &&  nameValueListEq nameValueList1 nameValueList2
     _ -> False
exprEq (FunctionCallExpressionList e1 e1' _) (FunctionCallExpressionList e2 e2' _) =
   case (e1', e2') of
     (Nothing, Nothing) -> exprEq e1 e2
     (Just expressionList1, Just expressionList2) -> exprEq e1 e2 &&  expressionListEq expressionList1 expressionList2
     _ -> False
exprEq (MemberAccess e1 i1 _) (MemberAccess e2 i2 _) = exprEq e1 e2  && eqIdentifier i1 i2
exprEq (Literal e1) (Literal e2) = eqPrimaryExpression e1 e2
exprEq (New e1 _) (New e2 _) = eqTypeName e1 e2
exprEq _ _ = False

maybeExprEq :: Maybe (Expression SourceRange) -> Maybe (Expression SourceRange) -> Bool
maybeExprEq Nothing Nothing  = True
maybeExprEq Nothing (Just _) = False
maybeExprEq (Just _) Nothing = False
maybeExprEq (Just a) (Just b) = exprEq a b

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
exprExistsInExpr e e'@(Unary _ ue _) = exprEq e e' || exprExistsInExprEx e ue
exprExistsInExpr e e'@(Binary _ lhs rhs _) = exprEq e e' || exprExistsInExprEx e lhs || exprExistsInExprEx e rhs
exprExistsInExpr e e'@(Ternary _ cond lhs rhs _) = exprEq e e' || exprExistsInExprEx e cond || exprExistsInExprEx e lhs || exprExistsInExprEx e rhs
exprExistsInExpr e e'@(FunctionCallNameValueList f pl _) =
   exprEq e e' || exprExistsInExprEx e f || case pl of
    Nothing -> False
    Just (NameValueList ns) -> foldr ((||) . exprExistsInExpr e . snd) False ns
exprExistsInExpr e e'@(FunctionCallExpressionList f pl _) =
   exprEq e e' || exprExistsInExprEx e f || case pl of
    Nothing -> False
    Just (ExpressionList ns) -> foldr ((||) . exprExistsInExpr e) False ns
exprExistsInExpr e e'@(MemberAccess me _ _) = exprEq e  e' || exprExistsInExprEx e me
exprExistsInExpr e e'@(Literal l) =
  exprEq e e' || case l of
    PrimaryExpressionTupleExpression (RoundBrackets es _) -> foldr ((||) . exprExistsInExpr e) False es
    PrimaryExpressionTupleExpression (SquareBrackets es _) -> foldr ((||) . exprExistsInExpr e) False es
    _ -> False
exprExistsInExpr e e' = exprEq e e'


exprExistsInExprEx :: Expression SourceRange -> Expression SourceRange -> Bool
exprExistsInExprEx decExpr ancExpr = decExpr == ancExpr || exprExistsInExpr decExpr ancExpr


defaultSourceRange :: SourceRange
defaultSourceRange = SourceRange (newPos "" 0 0) (newPos "" 0 0)
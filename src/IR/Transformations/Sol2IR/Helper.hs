
module IR.Transformations.Sol2IR.Helper where
import Text.Parsec.Pos

import Solidity.Spec as Sol

import IR.Spec as IR
import IR.Transformations.Base
import Data.Maybe (catMaybes)


eqIdentifier :: Identifier SourceRange -> Identifier SourceRange -> (Bool, SourceRange)
eqIdentifier (Sol.Identifier iid _) (Sol.Identifier iid' a) = (iid == iid', a)

eqStateMutability :: StateMutability SourceRange -> StateMutability SourceRange -> (Bool, SourceRange)
eqStateMutability (StateMutability ability _) (StateMutability ability' a) = (ability == ability', a)

compareList :: [a] -> [a] -> (a -> a -> (Bool, SourceRange)) -> Bool
compareList [] [] _ = True
compareList [] ((_:_)) _ = False
compareList ((_:_)) [] _ = False
compareList ((e:xs) ) ((e':ys)) fn = fst (fn e e') && compareList xs ys fn


eqPrimaryExpression :: PrimaryExpression SourceRange -> PrimaryExpression SourceRange -> (Bool, SourceRange)
eqPrimaryExpression (PrimaryExpressionBooleanLiteral (BooleanLiteral v _)) (PrimaryExpressionBooleanLiteral (BooleanLiteral v' a)) = (v == v', a)
eqPrimaryExpression (PrimaryExpressionNumberLiteral (NumberLiteral v _)) (PrimaryExpressionNumberLiteral (NumberLiteral v' a)) = (v == v', a)
eqPrimaryExpression (PrimaryExpressionHexLiteral (HexLiteral v _)) (PrimaryExpressionHexLiteral (HexLiteral v' a)) = (v == v', a)
eqPrimaryExpression (PrimaryExpressionStringLiteral (Sol.StringLiteral v _)) (PrimaryExpressionStringLiteral (Sol.StringLiteral v' a)) = (v == v', a)
eqPrimaryExpression (PrimaryExpressionTupleExpression (RoundBrackets v _)) (PrimaryExpressionTupleExpression (RoundBrackets v' a))
  = (compareList v v' exprEq, a)
eqPrimaryExpression (PrimaryExpressionTupleExpression (SquareBrackets v _)) (PrimaryExpressionTupleExpression (SquareBrackets v' a))
  = (compareList v v' exprEq, a)
eqPrimaryExpression (PrimaryExpressionIdentifier (Sol.Identifier v _)) (PrimaryExpressionIdentifier (Sol.Identifier v' a))
  = (v == v', a)
eqPrimaryExpression (PrimaryExpressionElementaryTypeNameExpression et) (PrimaryExpressionElementaryTypeNameExpression et')
  = eqElementaryTypeName et et'

eqPrimaryExpression _ _ = (False, defaultSourceRange)


eqElementaryTypeName :: ElementaryTypeName  SourceRange -> ElementaryTypeName  SourceRange -> (Bool, SourceRange)
eqElementaryTypeName (ElementaryTypeName t _) (ElementaryTypeName t' a) = (t == t', a)


eqUserDefinedTypeName :: UserDefinedTypeName  SourceRange -> UserDefinedTypeName  SourceRange -> Bool
eqUserDefinedTypeName (UserDefinedTypeName l) (UserDefinedTypeName l')  = compareList l l' eqIdentifier


eqTypeName :: TypeName SourceRange -> TypeName SourceRange -> (Bool, SourceRange)
eqTypeName (TypeNameMapping et tn _) (TypeNameMapping et' tn' a) = (fst (eqElementaryTypeName et et') &&  fst (eqTypeName tn tn'), a)
eqTypeName (TypeNameFunctionTypeName l stateMutability ml _) (TypeNameFunctionTypeName l' stateMutability' ml' a) =
   (typeNameListEq l l' && compareList stateMutability stateMutability' eqStateMutability && maybeTypeNameListEq ml ml', a)
eqTypeName (TypeNameElementaryTypeName et _) (TypeNameElementaryTypeName et' a) = (fst (eqElementaryTypeName et et'), a)
eqTypeName (TypeNameUserDefinedTypeName l _) (TypeNameUserDefinedTypeName l' a) = (eqUserDefinedTypeName l l', a)
eqTypeName (TypeNameArrayTypeName t me _) (TypeNameArrayTypeName t' me' a) = (fst (eqTypeName t t') &&  maybeExprEq me me', a)
eqTypeName _ _ = (False, defaultSourceRange)

typeNameListEq :: TypeNameList SourceRange -> TypeNameList SourceRange -> Bool
typeNameListEq (TypeNameList l) (TypeNameList l')  = compareList l l' eqTypeName

maybeTypeNameListEq :: Maybe (TypeNameList SourceRange) -> Maybe (TypeNameList SourceRange) -> Bool
maybeTypeNameListEq Nothing Nothing  = True
maybeTypeNameListEq Nothing (Just _) = False
maybeTypeNameListEq (Just _) Nothing = False
maybeTypeNameListEq (Just a) (Just b) = typeNameListEq a b


eqNameValue :: (Identifier SourceRange, Expression SourceRange) -> (Identifier SourceRange, Expression SourceRange) -> (Bool, SourceRange)
eqNameValue (i, e) (i', e') = (fst r1 &&  fst r2, mergeRange (snd r1) (snd r2)) where
                                    r1 = eqIdentifier i i'
                                    r2 = exprEq e e'

nameValueListEq :: NameValueList SourceRange -> NameValueList SourceRange -> Bool
nameValueListEq (NameValueList l) (NameValueList l')  = compareList l l' eqNameValue

expressionListEq :: ExpressionList SourceRange -> ExpressionList SourceRange -> Bool
expressionListEq (ExpressionList l) (ExpressionList l')  = compareList l l' exprEq


exprEq :: Expression SourceRange -> Expression SourceRange -> (Bool, SourceRange)
exprEq (Unary op1 e1 _) (Unary op2 e2 a) = (op1 == op2 && fst (exprEq e1 e2), a)
exprEq (Binary op1 e1 e1' _) (Binary op2 e2 e2' a) = (op1 == op2 && fst (exprEq e1 e2) && fst ( exprEq e1' e2'), a)
exprEq (Ternary op1 e1 e1' e1'' _) (Ternary op2 e2 e2' e2'' a) = (op1 == op2 && fst (exprEq e1 e2) && fst (exprEq e1' e2') && fst (exprEq e1'' e2''), a)
exprEq (FunctionCallNameValueList e1 e1' _) (FunctionCallNameValueList e2 e2' a) =
   case (e1', e2') of
     (Nothing, Nothing) -> exprEq e1 e2
     (Just nameValueList1, Just nameValueList2) -> (fst (exprEq e1 e2) &&  nameValueListEq nameValueList1 nameValueList2, a)
     _ -> (False, a)
exprEq (FunctionCallExpressionList e1 e1' _) (FunctionCallExpressionList e2 e2' a) =
   case (e1', e2') of
     (Nothing, Nothing) -> exprEq e1 e2
     (Just expressionList1, Just expressionList2) -> (fst (exprEq e1 e2) &&  expressionListEq expressionList1 expressionList2, a)
     _ -> (False, a)
exprEq (MemberAccess e1 i1 _) (MemberAccess e2 i2 a) = ( fst (exprEq e1 e2)  && fst (eqIdentifier i1 i2), a)
exprEq (Literal e1) (Literal e2) = eqPrimaryExpression e1 e2
exprEq (New e1 _) (New e2 a) = (fst (eqTypeName e1 e2), a)
exprEq _ _ = (False, defaultSourceRange)

maybeExprEq :: Maybe (Expression SourceRange) -> Maybe (Expression SourceRange) -> Bool
maybeExprEq Nothing Nothing  = True
maybeExprEq Nothing (Just _) = False
maybeExprEq (Just _) Nothing = False
maybeExprEq (Just a) (Just b) = fst $ exprEq a b

exprExistsInStmt :: Expression SourceRange -> Statement SourceRange -> (Bool, SourceRange)
exprExistsInStmt e (BlockStatement (Sol.Block stmts a)) = foldr (\ (exists, src) s -> if fst s then s else (exists, src)) (False, a) ts
                                                        where
                                                          ts = map (exprExistsInStmt e) stmts

exprExistsInStmt e (IfStatement cond trueBranch maybeFalseBranch _)
  | fst r1 = r1
  | fst r2 = r2
  | otherwise = r3
  where
      r1 = exprExistsInExpr e cond
      r2 = exprExistsInStmt e trueBranch
      r3 = case maybeFalseBranch of
                Nothing -> (False, defaultSourceRange)
                Just fb -> exprExistsInStmt e fb
exprExistsInStmt e (SimpleStatementExpression e' _) = exprExistsInExpr e e'
exprExistsInStmt e (SimpleStatementVariableDeclarationList _ rhs a) = foldr (\ (exists, src) s -> if fst s then s else (exists, src)) (False, a) ts
                                                        where
                                                          ts = map (exprExistsInExpr e) rhs

exprExistsInStmt e (SimpleStatementVariableAssignmentList _ rhs a) = foldr (\ (exists, src) s -> if fst s then s else (exists, src)) (False, a) ts
                                                        where
                                                          ts = map (exprExistsInExpr e) rhs
exprExistsInStmt _ a = (False, ann a)


exprExistsInExpr :: Expression SourceRange -> Expression SourceRange -> (Bool, SourceRange)
exprExistsInExpr e e'@(Unary _ ue _)
  | fst r1 = r1
  | otherwise = r2
  where
      r1 = exprEq e e'
      r2 = exprExistsInExprEx e ue
exprExistsInExpr e e'@(Binary _ lhs rhs _)
  | fst r1 = r1
  | fst r2 = r2
  | otherwise = r3
  where
      r1 = exprEq e e'
      r2 = exprExistsInExprEx e lhs
      r3 = exprExistsInExprEx e rhs
exprExistsInExpr e e'@(Ternary _ cond lhs rhs _)
  | fst r1 = r1
  | fst r2 = r2
  | fst r3 = r3
  | otherwise = r4
  where
      r1 = exprEq e e'
      r2 = exprExistsInExprEx e cond
      r3 = exprExistsInExprEx e lhs
      r4 = exprExistsInExprEx e rhs
exprExistsInExpr e e'@(FunctionCallNameValueList f pl a)
  | fst r1 = r1
  | fst r2 = r2
  | otherwise = r3
  where
      r1 = exprEq e e'
      r2 = exprExistsInExprEx e f
      r3 = case pl of
        Nothing -> (False, a)
        Just (NameValueList ns) -> foldr (\ (exists, src) s -> if fst s then s else (exists, src)) (False, a) ts
                                                        where
                                                          ts = map (exprExistsInExpr e . snd) ns

exprExistsInExpr e e'@(FunctionCallExpressionList f pl a)
  | fst r1 = r1
  | fst r2 = r2
  | otherwise = r3
  where
      r1 = exprEq e e'
      r2 = exprExistsInExprEx e f
      r3 = case pl of
        Nothing -> (False, a)
        Just (ExpressionList ns) -> foldr (\ (exists, src) s -> if fst s then s else (exists, src)) (False, a) ts
                                                        where
                                                          ts = map (exprExistsInExpr e) ns


exprExistsInExpr e e'@(MemberAccess me _ _) 
  | fst r1 = r1
  | otherwise = r2
  where
      r1 = exprEq e e'
      r2 = exprExistsInExprEx e me

exprExistsInExpr e e'@(Literal l)
  | fst r1 = r1
  | otherwise = r2
  where
      r1 = exprEq e e'
      r2 = case l of
        PrimaryExpressionTupleExpression (RoundBrackets es a) -> foldr (\ (exists, src) s -> if fst s then s else (exists, src)) (False, a) ts
                                                        where
                                                          ts = map (exprExistsInExpr e) es

        PrimaryExpressionTupleExpression (SquareBrackets es a) -> foldr (\ (exists, src) s -> if fst s then s else (exists, src)) (False, a) ts
                                                        where
                                                          ts = map (exprExistsInExpr e) es
        _ -> (False, defaultSourceRange)

exprExistsInExpr e e' = exprEq e e'


exprExistsInExprEx :: Expression SourceRange -> Expression SourceRange -> (Bool, SourceRange)
exprExistsInExprEx decExpr ancExpr = if fst r1 then r1 else r2
                                      where
                                        r1 = exprEq decExpr ancExpr
                                        r2 = exprExistsInExpr decExpr ancExpr


defaultSourceRange :: SourceRange
defaultSourceRange = SourceRange (newPos "" 0 0) (newPos "" 0 0)

-- get default value for a type
defaultValueExpr :: IType' -> Transformation IExpression'
defaultValueExpr Nothing = return Nothing 
defaultValueExpr (Just (ElementaryType IR.Int)) = return $ Just $ LiteralExpr $ IntLiteral False 0
defaultValueExpr (Just (ElementaryType IR.Bool)) = return $ Just $ LiteralExpr $ BoolLiteral False
defaultValueExpr (Just (ElementaryType IR.Bytes)) = return $ Just $ LiteralExpr $ BytesLiteral []
defaultValueExpr (Just (ElementaryType IR.Address)) = return $ Just $ FunctionCallExpr (IdentifierExpr (IR.ReservedId "Ripemd160")) [LiteralExpr $ BytesLiteral $ replicate 20 0]
defaultValueExpr (Just (ElementaryType IR.String)) = return $ Just $ LiteralExpr $ IR.StringLiteral ""
defaultValueExpr (Just (UserDefinedType n)) = do
  st' <- lookupStruct n
  case st' of
    Nothing -> return Nothing
    Just st -> do
      fields <- mapM (\(Param t _) -> defaultValueExpr (Just t)) (structFields st)
      return $ Just $ StructLiteralExpr $ catMaybes fields
defaultValueExpr (Just (Array arr n)) = do
  e <- defaultValueExpr (Just arr)
  case e of
    Just e' -> return $ Just $ ArrayLiteralExpr $  replicate n e'
    _ -> return Nothing 
defaultValueExpr _ = return Nothing
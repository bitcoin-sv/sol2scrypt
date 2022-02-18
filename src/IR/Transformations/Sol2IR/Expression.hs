{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module IR.Transformations.Sol2IR.Expression where

import Control.Monad.State
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import IR.Spec as IR
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Identifier (maybeStateVarId)
import Numeric
import Solidity.Spec as Sol
import Utils

-- only used by Transfer array sub. eg. int[20]
instance ToIRTransformable Sol.Expression Int where
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec n _))) = return (fst $ head $ readDec n)
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteralHex n _))) = return (fst $ head $ readHex n)
  _toIR e = error $ "unsupported expression to Integer : `" ++ show e ++ "`"

instance ToIRTransformable (Maybe Sol.Expression) Int where
  _toIR (Just e) = _toIR e
  _toIR e = error $ "unsupported expression to Integer : `" ++ show e ++ "`"

instance ToIRTransformable (Maybe Sol.Expression) IExpression' where
  _toIR (Just e) = _toIR e
  _toIR Nothing = return Nothing

instance ToIRTransformable Sol.Expression IExpression' where
  _toIR (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral b))) =
    return $ Just $ LiteralExpr $ IR.BoolLiteral ("true" == toLower b)
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteralHex n Nothing))) =
    return $ Just $ LiteralExpr $ IR.IntLiteral True (fst $ head $ readHex n)
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec n _))) =
    return $ Just $ LiteralExpr $ IR.IntLiteral False (fst $ head $ readDec n)
  _toIR (Literal (PrimaryExpressionHexLiteral (HexLiteral h))) =
    return $ Just $ LiteralExpr $ IR.BytesLiteral $ parseHex h
  _toIR (Literal (PrimaryExpressionStringLiteral (Sol.StringLiteral s))) =
    return $ Just $ LiteralExpr $ IR.StringLiteral s
  _toIR (Literal (PrimaryExpressionIdentifier i)) = do
    i' <- _toIR i
    i'' <- maybeStateVarId i'
    return $ IdentifierExpr <$> i''
  _toIR (Unary opStr e) = do
    e' <- _toIR e
    return $ transformUnaryExpr opStr e'
  _toIR e@(Binary "[]" e1 e2) = do
    let arrayIndexAccess = \arr idx -> do
          arr' <- _toIR arr
          idx' <- _toIR idx
          return $ BinaryExpr Index <$> arr' <*> idx'
    baseId <- baseIdOfBinaryIndexExpr e
    case baseId of
      -- due to the lack of semantics, only `IdentifierExpr` can get resolved type here.
      Just i -> do
        i' <- lookupSym $ case i of
          IR.Identifier n -> IR.Identifier $ stripThis n
          ReservedId n -> ReservedId $ stripThis n
        case i' of
          -- mapping-typed var access
          Just (Symbol _ (Mapping _ vt) _) -> do
            mc <- gets stateInFuncMappingCounter
            let mapExpr = Just $ IdentifierExpr i
            keyExpr <- keyExprOfMapping e []
            modify $ \s -> s {stateInFuncMappingCounter = incExprCounter mc mapExpr keyExpr vt}
            -- transpile to a value expression
            let e' = BinaryExpr Index <$> mapExpr <*> keyExpr
            return $ valueExprOfMapping e' "" -- with empty postfix
          _ -> arrayIndexAccess e1 e2
      -- other exprs whose type is `Mapping` will not be correctly transpiled below due to the same above.
      _ -> arrayIndexAccess e1 e2
  _toIR (Binary opStr e1 e2) = do
    e1' <- _toIR e1
    e2' <- _toIR e2
    return $ BinaryExpr (str2BinaryOp opStr) <$> e1' <*> e2'
  _toIR (Ternary _ e1 e2 e3) = do
    e1' <- _toIR e1
    e2' <- _toIR e2
    e3' <- _toIR e3
    return $ TernaryExpr <$> e1' <*> e2' <*> e3'
  _toIR (Sol.MemberAccess (Literal (PrimaryExpressionIdentifier (Sol.Identifier "msg"))) (Sol.Identifier "sender")) = do
    return $ Just $ IR.IdentifierExpr (IR.ReservedId varMsgSender)
  _toIR (Sol.MemberAccess (Literal (PrimaryExpressionIdentifier (Sol.Identifier "msg"))) (Sol.Identifier "value")) = do
    return $ Just $ IR.IdentifierExpr (IR.ReservedId varMsgValue)
  _toIR (Sol.MemberAccess e i) = do
    e' <- _toIR e
    i' <- _toIR i
    return $ IR.MemberAccessExpr <$> e' <*> i'
  _toIR (FunctionCallExpressionList fe pl) = do
    fe' <- _toIR fe
    ps' <- case pl of
      Nothing -> return []
      Just (ExpressionList ps) -> mapM _toIR ps
    return $ FunctionCallExpr <$> fe' <*> sequence ps'
  _toIR (Literal (PrimaryExpressionTupleExpression (SquareBrackets array))) = do
    array' <- mapM _toIR array
    return $ Just $ ArrayLiteralExpr $ catMaybes array'
  _toIR e = error $ "unsupported expression : `" ++ headWord (show e) ++ "`"

transformUnaryExpr :: String -> IExpression' -> IExpression'
transformUnaryExpr opStr e' =
  case opStr of
    "-" -> UnaryExpr Negate <$> e'
    "()" -> ParensExpr <$> e'
    "()++" -> UnaryExpr PostIncrement <$> e'
    "++" -> UnaryExpr PreIncrement <$> e'
    "()--" -> UnaryExpr PostDecrement <$> e'
    "--" -> UnaryExpr PreDecrement <$> e'
    "!" -> UnaryExpr Not <$> e'
    s -> error $ "unsupported unary operator `" ++ s ++ "`"

str2BinaryOp :: String -> IBinaryOp
str2BinaryOp "+" = Add
str2BinaryOp "-" = Sub
str2BinaryOp "*" = Mul
str2BinaryOp "/" = Div
str2BinaryOp "%" = Mod
str2BinaryOp "+=" = AddAssign
str2BinaryOp "-=" = SubAssign
str2BinaryOp "*=" = MulAssign
str2BinaryOp "/=" = DivAssign
str2BinaryOp "%=" = ModAssign
str2BinaryOp "==" = IR.Equal
str2BinaryOp "!=" = Neq
str2BinaryOp "<" = LessThan
str2BinaryOp "<=" = LessThanOrEqual
str2BinaryOp ">" = GreaterThan
str2BinaryOp ">=" = GreaterThanOrEqual
str2BinaryOp "&&" = BoolAnd
str2BinaryOp "||" = BoolOr
str2BinaryOp "[]" = Index
str2BinaryOp s = error $ "unsupported binary operator `" ++ s ++ "`"

-- give an expression a var name which can be used in transformation, for example, declare a var for the expression.
toExprName :: IExpression -> ExprName
toExprName (LiteralExpr (BoolLiteral b)) = show b
toExprName (LiteralExpr (IntLiteral _ i)) = show i
toExprName (LiteralExpr (BytesLiteral bytes)) = concatMap showHexWithPadded bytes
toExprName (IdentifierExpr (IR.Identifier n)) = n
toExprName (IdentifierExpr (IR.ReservedId n)) = n
toExprName (BinaryExpr _ le re) = toExprName le ++ "_" ++ toExprName re
toExprName (StructLiteralExpr es) = intercalate "_" $ map toExprName es
toExprName e = error $ "the expr is not supported in #toExprName: " ++ show e

incExprCounter :: MappingExprCounter -> IExpression' -> IExpression' -> IType -> MappingExprCounter
incExprCounter ec (Just mapping) (Just key) et = Map.insert en (MECEntry et mapping key cnt) ec
  where
    en = toExprName $ BinaryExpr Index mapping key
    cnt = maybe 0 exprCnt $ Map.lookup en ec
incExprCounter ec _ _ _ = ec

-- get base identifier for binary index expression: `a[x][y]` -> `a`
baseIdOfBinaryIndexExpr :: Expression -> Transformation IIdentifier'
baseIdOfBinaryIndexExpr (Binary "[]" e@(Binary "[]" _ _) _) = baseIdOfBinaryIndexExpr e
baseIdOfBinaryIndexExpr (Binary "[]" (Literal (PrimaryExpressionIdentifier i)) _) = do
  i' <- _toIR i
  maybeStateVarId i'
baseIdOfBinaryIndexExpr _ = return Nothing

-- get expression for the key of mapping
keyExprOfMapping :: Expression -> [Expression] -> Transformation IExpression'
keyExprOfMapping (Binary "[]" e@(Binary "[]" _ _) ke) keys = keyExprOfMapping e $ ke : keys
keyExprOfMapping (Binary "[]" _ ke) keys =
  if null keys
    then do
      e <- _toIR ke
      let ke' = case toExprName <$> e of
            Just kn -> if kn `elem` reservedNames then Just (IR.ReservedId kn) else Just (IR.Identifier kn)
            _ -> Nothing
      -- use IdentifierExpr for non-nested-mapping key
      return $ IdentifierExpr <$> ke'
    else do
      keys' <- mapM _toIR $ ke : keys
      -- use StructLiteralExpr for nested-mapping keys
      return $ StructLiteralExpr <$> sequence keys'
keyExprOfMapping _ _ = return Nothing

-- name for mapping expression, e.x. use `mapping_key` as name of expr `mapping[key]`
valueNameOfMapping :: IExpression' -> String -> Maybe String
valueNameOfMapping e postfix = n'
  where
    n = replaceDotWithUnderscore . toExprName <$> e
    n' = (++) <$> n <*> Just postfix

-- use identifier expression to replace mapping expression
valueExprOfMapping :: IExpression' -> String -> IExpression'
valueExprOfMapping e@(Just (BinaryExpr Index _ _)) postfix = IdentifierExpr <$> (IR.Identifier <$> valueNameOfMapping e postfix)
valueExprOfMapping _ _ = Nothing

-- index name for mapping expression, e.x. use `mapping_key_index` as index name for expr `mapping[key]` 
indexNameOfMapping :: IExpression' -> String -> Maybe String
indexNameOfMapping e postfix = (++) <$> valName <*> Just "_index"
  where
    valName = valueNameOfMapping e postfix

-- corresponding index expression for mapping expression
indexExprOfMapping :: IExpression' -> String -> IExpression'
indexExprOfMapping e@(Just (BinaryExpr Index _ _)) postfix = IdentifierExpr <$> (IR.Identifier <$> indexNameOfMapping e postfix)
indexExprOfMapping _ _ = Nothing

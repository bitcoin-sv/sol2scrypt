{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IR.Transformations.Sol2IR.Expression where

import Numeric
import IR.Transformations.Base
import IR.Transformations.Sol2IR.Identifier (maybeStateVarId)
import Solidity.Spec as Sol
import IR.Spec as IR
import Utils
import Data.Maybe (catMaybes)



-- only used by Transfer array sub. eg. int[20]
instance ToIRTransformable Sol.Expression Int where
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec n _))) = return (fst $ head $ readDec n)
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteralHex n _))) = return (fst $ head $ readHex n)
  _toIR e = error $ "unsupported expression to Integer : `" ++ show e ++ "`"

instance ToIRTransformable (Maybe Sol.Expression) Int where
  _toIR (Just e)  = _toIR e
  _toIR e = error $ "unsupported expression to Integer : `" ++ show e ++ "`"

instance ToIRTransformable (Maybe Sol.Expression) IExpression' where
  _toIR (Just e)  = _toIR e
  _toIR Nothing  = return Nothing

instance ToIRTransformable Sol.Expression IExpression' where
  _toIR (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral b))) =
    return $ Just $ LiteralExpr $ IR.BoolLiteral ("true" == toLower b)
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteralHex n Nothing))) =
    return $ Just $ LiteralExpr $ IR.IntLiteral True (fst $ head $ readHex n)
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec n _))) =
    return $ Just $ LiteralExpr $ IR.IntLiteral False (fst $ head $ readDec n)
  _toIR (Literal (PrimaryExpressionHexLiteral (HexLiteral h))) =
    return $ Just $ LiteralExpr $ IR.BytesLiteral $ parseHex h
  _toIR (Literal (PrimaryExpressionIdentifier i)) = do
    i' <- _toIR i
    i'' <- maybeStateVarId i'
    return $ IdentifierExpr <$> i''
  _toIR (Unary opStr e) = do
    e' <- _toIR e
    return $ transformUnaryExpr opStr e'
  _toIR (Binary opStr e1 e2) = do
    e1' <- _toIR e1
    e2' <- _toIR e2
    return $ BinaryExpr (str2BinaryOp opStr) <$> e1' <*> e2'
  _toIR (Ternary _ e1 e2 e3) = do 
    e1' <- _toIR e1
    e2' <- _toIR e2 
    e3' <- _toIR e3 
    return $ TernaryExpr  <$> e1' <*> e2' <*> e3'
  _toIR (Sol.MemberAccess (Literal (PrimaryExpressionIdentifier (Sol.Identifier "msg"))) (Sol.Identifier "sender")) = do
    return $ Just $ IR.IdentifierExpr (IR.Identifier "msgSender")
  _toIR (Sol.MemberAccess (Literal (PrimaryExpressionIdentifier (Sol.Identifier "msg"))) (Sol.Identifier "value")) = do
    return $ Just $ IR.IdentifierExpr (IR.Identifier "msgValue")
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
str2BinaryOp s = error $ "unsupported binary operator `" ++ s ++ "`"
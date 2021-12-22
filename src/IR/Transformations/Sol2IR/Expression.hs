{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IR.Transformations.Sol2IR.Expression where

import Numeric
import IR.Transformations.Base
import Solidity.Spec as Sol
import IR.Spec as IR
import Utils

instance ToIRTransformable Sol.Expression IExpr' where
  _toIR (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral b))) = 
    return $ Just $ LiteralExpr $ IR.BoolLiteral ("true" == toLower b)
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteralHex n Nothing))) =
    return $ Just $ LiteralExpr $ IR.IntLiteral True (fst $ head $ readHex n)
  _toIR (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec n _))) =
    return $ Just $ LiteralExpr $ IR.IntLiteral False (fst $ head $ readDec n)
  _toIR (Literal (PrimaryExpressionHexLiteral (HexLiteral h))) =
    return $ Just $ LiteralExpr $ IR.BytesLiteral $ parseHex h
  _toIR (Literal (PrimaryExpressionIdentifier (Sol.Identifier i))) = return $ Just $ IdentifierExpr (IR.Identifier i)
  _toIR (Unary opStr e) = do 
    e' <- _toIR e 
    return $ transformUnaryExpr opStr e'
  _toIR (Binary opStr e1 e2) = do 
    e1' :: IExpr' <- _toIR e1
    e2' :: IExpr'  <- _toIR e2 
    return $ BinaryExpr (str2BinaryOp opStr) <$> e1' <*> e2' 
  _toIR _ = return Nothing -- ignore those which can not be transformed


transformUnaryExpr :: String -> IExpr' -> IExpr'
transformUnaryExpr opStr e' = 
  case opStr of
    "-" -> UnaryExpr Negate <$> e' 
    "()" -> Parens <$> e'
    s -> error $ "unsupported op `" ++ s ++ "`"

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
str2BinaryOp s = error $ "unsupported op `" ++ s ++ "`"
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
  _toIR (Unary opStr e) = do 
    e' <- _toIR e 
    return $ UnaryExpr (str2UnaryOp opStr) <$> e'
  _toIR (Binary opStr e1 e2) = do 
    e1' :: IExpr' <- _toIR e1
    e2' :: IExpr'  <- _toIR e2 
    return $ BinaryExpr (str2BinaryOp opStr) <$> e1' <*> e2' 
  _toIR _ = return Nothing -- ignore those which can not be transformed


str2UnaryOp :: String -> IUnaryOp
str2UnaryOp opStr = 
  case opStr of
    "-" -> Negate
    s -> error $ "unsupported op `" ++ s ++ "`"

str2BinaryOp :: String -> IBinaryOp
str2BinaryOp "+" = Add
str2BinaryOp s = error $ "unsupported op `" ++ s ++ "`"
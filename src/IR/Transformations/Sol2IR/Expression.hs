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
  _toIR _ = return Nothing

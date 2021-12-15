{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Intermediate.Transformations.Sol2IntM.Expression where

import Numeric
import Intermediate.Transformations.Base
import Solidity.Spec as Sol
import Intermediate.Spec as IntM
import Utils

instance ToIntermediateTransformable Sol.Expression IExpr' where
  _toIntermediate (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral b))) = 
    return $ Just $ LiteralExpr $ IntM.BoolLiteral ("true" == toLower b)
  _toIntermediate (Literal (PrimaryExpressionNumberLiteral (NumberLiteralHex n _))) =
    return $ Just $ LiteralExpr $ IntM.IntLiteral True (fst $ head $ readHex n)
  _toIntermediate (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec n _))) =
    return $ Just $ LiteralExpr $ IntM.IntLiteral False (fst $ head $ readDec n)
  _toIntermediate _ = return Nothing

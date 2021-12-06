{-# LANGUAGE ScopedTypeVariables #-}
module Intermediate.Transformations.Sol2IntM.Expression where

import Numeric
import Intermediate.Transformations.Base
import Solidity.Spec as Sol
import Intermediate.Spec as IntM
import Control.Monad.State
import Utils

-----------------  Solidity to Intermediate -----------------

transformSolExpression :: String -> IO IExpr'
transformSolExpression solidityCode = do
  expr :: Expression <- parseIO solidityCode
  fst <$> runStateT (solExpression2Intermediate expr) TransformState

solExpression2Intermediate :: Sol.Expression -> Transformation IExpr'
-- Literal PrimaryExpression
solExpression2Intermediate (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral b))) = 
  return $ Just $ IntM.BoolLiteral ("true" == toLower b)
solExpression2Intermediate (Literal (PrimaryExpressionNumberLiteral (NumberLiteralHex n _))) =
  return $ Just $ IntM.IntLiteral True (fst $ head $ readHex n)
solExpression2Intermediate (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec n _))) =
  return $ Just $ IntM.IntLiteral False (fst $ head $ readDec n)
solExpression2Intermediate _ = return Nothing
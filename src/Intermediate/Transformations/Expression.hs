{-# LANGUAGE ScopedTypeVariables #-}
module Intermediate.Transformations.Expression where

import Numeric
import Intermediate.Transformations.Base
import Solidity.Spec as Sol
import Intermediate.Spec as IntM
import Scrypt.Spec as Scr
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


-----------------  Intermediate to sCrypt  -----------------

transformIntermediateExpr :: IExpr -> Scr.Expr IExpr
transformIntermediateExpr e@(IntM.BoolLiteral b) = Scr.BoolLiteral b e
transformIntermediateExpr e@(IntM.IntLiteral _isHex i) = Scr.IntLiteral _isHex i e
transformIntermediateExpr e = error $ "transformIntermediateExpr for `" ++ show e ++ "` not implemented in scrypt"

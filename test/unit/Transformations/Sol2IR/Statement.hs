module Transformations.Sol2IR.Statement where

import Solidity.Spec as Sol
import IR.Spec as IR
import IR.Transformer
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable Sol.Statement IExpr'" $ do
  describe "#ExprStmt" $ do
    it "should transfrom Solidity `BoolLiteral` to IR Statement correctly" $ do
      r1 <- transform2IR TransformState $ SimpleStatementExpression (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral "true")))
      r1 `shouldBe` Just (ExprStmt (LiteralExpr $ IR.BoolLiteral True))
      r2 <- transform2IR TransformState $ SimpleStatementExpression (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral "false")))
      r2 `shouldBe` Just (ExprStmt (LiteralExpr $ IR.BoolLiteral False))

    it "should transfrom Solidity `NumberLiteral` to IR Statement correctly" $ do
      r1 <- transform2IR TransformState $ SimpleStatementExpression (Literal (PrimaryExpressionNumberLiteral (NumberLiteralHex "0123abcdef" Nothing)))
      r1 `shouldBe` Just (ExprStmt (LiteralExpr $ IR.IntLiteral True 4893429231))
      r2 <- transform2IR TransformState $ SimpleStatementExpression (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "12345" Nothing)))
      r2 `shouldBe` Just (ExprStmt (LiteralExpr $ IR.IntLiteral False 12345))

  describe "#AssignStmt" $ do
    it "should transfrom Solidity `BoolLiteral` to IR Statement correctly" $ do
      r1 <- transform2IR TransformState $ SimpleStatementVariableAssignmentList [Just (Sol.Identifier {Sol.unIdentifier = "x"})] [Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "11" Nothing))]
      r1 `shouldBe` Just (AssignStmt [Just $ IR.Identifier "x"] [LiteralExpr $ IR.IntLiteral False 11])

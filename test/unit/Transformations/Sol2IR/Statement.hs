{-# LANGUAGE FlexibleContexts #-}

module Transformations.Sol2IR.Statement where

import IR.Spec as IR
import IR.Transformer
import Solidity.Spec as Sol
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable Sol.Statement IExpr'" $ do
  let itstmt title e1 e2 = it ("should transfrom Solidity `" ++ title ++ "` to IR Statement correctly") $ do
        r1 <- transform2IR (TransformState []) e1
        r1 `shouldBe` Just e2

  describe "#SimpleStatementExpression" $ do
    itstmt
      "BoolLiteral"
      (SimpleStatementExpression (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral "true"))))
      (ExprStmt (LiteralExpr $ IR.BoolLiteral True))

    itstmt
      "BoolLiteral"
      (SimpleStatementExpression (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral "false"))))
      (ExprStmt (LiteralExpr $ IR.BoolLiteral False))

    itstmt
      "NumberLiteral"
      (SimpleStatementExpression (Literal (PrimaryExpressionNumberLiteral (NumberLiteralHex "0123abcdef" Nothing))))
      (ExprStmt (LiteralExpr $ IR.IntLiteral True 4893429231))

    itstmt
      "NumberLiteral"
      (SimpleStatementExpression (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "12345" Nothing))))
      (ExprStmt (LiteralExpr $ IR.IntLiteral False 12345))

    itstmt
      "NumberLiteral"
      (SimpleStatementExpression (Literal (PrimaryExpressionNumberLiteral (NumberLiteralHex "0123abcdef" Nothing))))
      (ExprStmt (LiteralExpr $ IR.IntLiteral True 4893429231))

    itstmt
      "HexLiteral"
      (SimpleStatementExpression (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "12345" Nothing))))
      (ExprStmt (LiteralExpr $ IR.IntLiteral False 12345))

    itstmt
      "Unary"
      (SimpleStatementExpression (Unary "-" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing)))))
      (ExprStmt (UnaryExpr Negate (LiteralExpr $ IR.IntLiteral False 100)))


    itstmt
      "Binary"
      (SimpleStatementExpression (Binary "*" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing)))))
      (ExprStmt (BinaryExpr IR.Mul (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1)))

    describe "#AssignStmt" $ do
      itstmt 
        "IntLiteral"
        (SimpleStatementVariableAssignmentList [Just (Sol.Identifier {Sol.unIdentifier = "x"})] [Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "11" Nothing))])
        (IR.AssignStmt [Just $ IR.Identifier "x"] [LiteralExpr $ IR.IntLiteral False 11])

      itstmt 
        "IntLiteral"
        (SimpleStatementVariableAssignmentList [Just (Sol.Identifier {Sol.unIdentifier = "x"})] [Literal (PrimaryExpressionNumberLiteral (NumberLiteralHex "11" Nothing))])
        (IR.AssignStmt [Just $ IR.Identifier "x"] [LiteralExpr $ IR.IntLiteral True 17])

      itstmt
        "BooleanLiteral"
        (SimpleStatementVariableAssignmentList [Just (Sol.Identifier {Sol.unIdentifier = "x"})] [Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral "true"))])
        (IR.AssignStmt [Just $ IR.Identifier "x"] [LiteralExpr $ IR.BoolLiteral True])
    describe "#AssignStmt" $ do
      it "should transfrom Solidity `BoolLiteral` to IR Statement correctly" $ do
        r1 <- transform2IR (TransformState []) $ SimpleStatementVariableAssignmentList [Just (Sol.Identifier {Sol.unIdentifier = "x"})] [Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "11" Nothing))]
        r1 `shouldBe` Just (AssignStmt [Just $ IR.Identifier "x"] [LiteralExpr $ IR.IntLiteral False 11])

    describe "#DeclareStmt" $ do
      it "should transfrom Solidity `BoolLiteral` to IR Statement correctly" $ do
        let declare = Sol.VariableDeclaration (TypeNameElementaryTypeName BoolType) Nothing (Sol.Identifier {Sol.unIdentifier = "x"})
            e = Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral "true"))
        r1 <- transform2IR (TransformState []) $ SimpleStatementVariableDeclarationList [Just declare] [e]
        r1 `shouldBe` Just (DeclareStmt [Just $ IR.Param (ElementaryType Bool) (IR.Identifier "x")] [LiteralExpr $ IR.BoolLiteral True])

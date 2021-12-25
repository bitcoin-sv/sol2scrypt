{-# LANGUAGE FlexibleContexts #-}

module Transformations.IR2Scr.Stmt where

import IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils

spec :: IO TestTree
spec = testSpec "instance ToScryptTransformable IStatment (Scr.Statement IExpr)" $ do
  let itExprStmt title e1 e2 = it ("should transfrom IR `" ++ title ++ "` to sCrypt Statement correctly") $ do
        r1 <- transform2Scrypt $ Just . IR.ExprStmt $ e1
        r1 `shouldBe` Just (Scr.ExprStmt e2 nil)

  describe "#ExprStmt" $ do
    itExprStmt
      "BoolLiteral"
      (LiteralExpr $ IR.BoolLiteral True)
      (Scr.BoolLiteral True nil)

    itExprStmt
      "BoolLiteral"
      (LiteralExpr $ IR.BoolLiteral False)
      (Scr.BoolLiteral False nil)

    itExprStmt
      "IntLiteral"
      (LiteralExpr $ IR.IntLiteral True 15)
      (Scr.IntLiteral True 15 nil)

    itExprStmt
      "HexLiteral"
      (LiteralExpr $ IR.BytesLiteral [1, 1, 19])
      (Scr.BytesLiteral [1, 1, 19] nil)

    itExprStmt
      "UnaryExpr"
      (IR.UnaryExpr IR.PreDecrement $ IR.IdentifierExpr $ IR.Identifier "a")
      (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False nil) nil)

    itExprStmt
      "BinaryExpr"
      (IR.BinaryExpr IR.Mul (LiteralExpr $ IR.IntLiteral True 15) (LiteralExpr $ IR.IntLiteral True 15))
      (Scr.BinaryExpr Scr.Mul (Scr.IntLiteral True 15 nil) (Scr.IntLiteral True 15 nil) nil)

    describe "#AssignStmt" $ do

      let itAssignStmt title e1 e2 = it ("should transfrom IR `" ++ title ++ "` to sCrypt Statement correctly") $ do
            let idtf = Identifier "x" 
            r1 <- transform2Scrypt $ Just $ IR.AssignStmt [Just idtf] [e1]
            r1 `shouldBe` Just (Scr.Assign (Var "x" False nil) e2 nil)

      itAssignStmt "IntLiteral" (LiteralExpr $ IR.IntLiteral True 15)
        (Scr.IntLiteral True 15 nil)

      itAssignStmt "IntLiteral" (LiteralExpr $ IR.IntLiteral False 15)
        (Scr.IntLiteral False 15 nil)

      itAssignStmt "BoolLiteral" (LiteralExpr $ IR.BoolLiteral True)
        (Scr.BoolLiteral True nil)

      itAssignStmt "HexLiteral" (LiteralExpr $ IR.BytesLiteral [1,1,19])
        (Scr.BytesLiteral [1,1,19] nil)



    -- describe "#DeclareStmt" $ do
    --   it "should transform IR `IntLiteral` to sCrypt Statment correctly" $ do
    --     let idtf = Identifier "x"
    --         declare = IR.Param (ElementaryType IR.Bool) idtf
    --         e1 = LiteralExpr $ IR.BoolLiteral True
    --         param = Scr.Param (TypeAnn Scr.Bool nil) (NameAnn "x" nil) (Const False) Nothing Scr.Public (IsStateProp False) nil
    --     r1 <- transform2Scrypt $ Just $ IR.DeclareStmt [Just declare] [e1]
    --     r1 `shouldBe` Just (Scr.Declare param (IR.BoolLiteral True) nil)

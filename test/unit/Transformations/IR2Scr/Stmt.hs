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
          r1 `shouldBe` Just (Scr.ExprStmt e2 e1)

  describe "#ExprStmt" $ do

    itExprStmt "BoolLiteral" (LiteralExpr $ IR.BoolLiteral True) (Scr.BoolLiteral True (LiteralExpr $ IR.BoolLiteral True))

    itExprStmt "BoolLiteral" (LiteralExpr $ IR.BoolLiteral False) (Scr.BoolLiteral False (LiteralExpr $ IR.BoolLiteral False))

    itExprStmt "IntLiteral" (LiteralExpr $ IR.IntLiteral True 15) (Scr.IntLiteral True 15 (LiteralExpr $ IR.IntLiteral True 15))

    itExprStmt "HexLiteral" (LiteralExpr $ IR.BytesLiteral [1,1,19]) (Scr.BytesLiteral [1,1,19] (LiteralExpr $ IR.BytesLiteral [1,1,19]))

    let e1 = IR.IdentifierExpr $ IR.IIdentifier "a"
    let e = IR.UnaryExpr IR.PreDecrement e1
    itExprStmt "UnaryExpr" e (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False e1) e)

    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.Mul  e1 e2
    itExprStmt "BinaryExpr" e (Scr.BinaryExpr Scr.Mul (Scr.IntLiteral True 15 e1) (Scr.IntLiteral True 15 e2) e)

    describe "#AssignStmt2" $ do
      it "should transform IR `IntLiteral` to sCrypt Statment correctly" $ do
        let idtf = Identifier "x" 
            e1 = LiteralExpr $ IR.IntLiteral True 15
        r1 <- transform2Scrypt $ Just $ IR.AssignStmt [Just idtf] [e1]
        r1 `shouldBe` Just (Scr.Assign (Var "x" False nil) (Scr.IntLiteral True 15 nil) nil)
    -- itExprStmt "IntLiteral" e (Scr.IntLiteral True 15 e) where e = (LiteralExpr $ IR.IntLiteral True 15)

    -- itExprStmt "IntLiteral" e (Scr.IntLiteral False 15 e) where e = (LiteralExpr $ IR.IntLiteral False 15)



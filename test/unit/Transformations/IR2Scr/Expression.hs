{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformations.IR2Scr.Expression where

import IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils

spec :: IO TestTree
spec = testSpec "instance ToScryptTransformable IExpr (Scr.Expr Ann)" $ do
  let itExpr title e c = it ("should transform IR  `" ++ title ++ "` to sCrypt Expression correctly") $ do
        r1 <- transform2Scrypt $ Just e
        r1 `shouldBe` Just c

  describe "#BoolLiteral" $ do
    itExpr
      "BoolLiteral"
      (LiteralExpr $ IR.BoolLiteral True)
      (Scr.BoolLiteral True nil)
    itExpr
      "BoolLiteral"
      (LiteralExpr $ IR.BoolLiteral False)
      (Scr.BoolLiteral False nil)

  describe "#IntLiteral" $ do
    itExpr
      "IntLiteral"
      (LiteralExpr $ IR.IntLiteral True 15)
      (Scr.IntLiteral True 15 nil)
    itExpr
      "IntLiteral"
      (LiteralExpr $ IR.IntLiteral False 15)
      (Scr.IntLiteral False 15 nil)

  describe "#BytesLiteral" $ do
    itExpr
      "BytesLiteral"
      (LiteralExpr $ IR.BytesLiteral [1, 1, 19])
      (Scr.BytesLiteral False [1, 1, 19] nil)
    itExpr
      "BytesLiteral"
      (LiteralExpr $ IR.BytesLiteral [])
      (Scr.BytesLiteral False [] nil)

  describe "#Parens" $ do
    itExpr
      "Parens"
      (IR.ParensExpr $ LiteralExpr $ IR.IntLiteral True 15)
      (Scr.Parens (Scr.IntLiteral True 15 nil) nil)

  describe "#UnaryExpr" $ do
    itExpr
      "-"
      (IR.UnaryExpr IR.Negate (LiteralExpr $ IR.IntLiteral True 15))
      (Scr.UnaryExpr Scr.Negate (Scr.IntLiteral True 15 nil) nil)

    itExpr
      "++"
      (IR.UnaryExpr IR.PreIncrement (IR.IdentifierExpr $ IR.Identifier "a"))
      (Scr.UnaryExpr Scr.PreIncrement (Scr.Var "a" False nil) nil)

    itExpr
      "()++"
      (IR.UnaryExpr IR.PostIncrement (IR.IdentifierExpr $ IR.Identifier "a"))
      (Scr.UnaryExpr Scr.PostIncrement (Scr.Var "a" False nil) nil)

    itExpr
      "--"
      (IR.UnaryExpr IR.PreDecrement (IR.IdentifierExpr $ IR.Identifier "a"))
      (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False nil) nil)

    itExpr
      "()--"
      (IR.UnaryExpr IR.PostDecrement (IR.IdentifierExpr $ IR.Identifier "a"))
      (Scr.UnaryExpr Scr.PostDecrement (Scr.Var "a" False nil) nil)

    itExpr
      "!"
      (IR.UnaryExpr IR.Not (IR.IdentifierExpr $ IR.Identifier "a"))
      (Scr.UnaryExpr Scr.Not (Scr.Var "a" False nil) nil)

  describe "#BinaryExpr" $ do
    let itBinary op op' = it ("should transform IR  `" ++ op ++ "` to sCrypt BinaryExpr correctly") $ do
          r1 <- transform2Scrypt $ Just (IR.BinaryExpr op' (LiteralExpr $ IR.IntLiteral True 15) (LiteralExpr $ IR.IntLiteral True 15))
          r1 `shouldBe` Just (Scr.BinaryExpr (toScryptBinaryOp op') (Scr.IntLiteral True 15 nil) (Scr.IntLiteral True 15 nil) nil)

    let itBinary' op op' = it ("should transform IR  `" ++ op ++ "` to sCrypt BinaryExpr correctly") $ do
          r1 <- transform2Scrypt $ Just (IR.BinaryExpr op' (IdentifierExpr $ IR.Identifier "a") (LiteralExpr $ IR.IntLiteral True 15))
          r1 `shouldBe` Just (Scr.BinaryExpr (toScryptBinaryOp  op') (Scr.Var "a" False nil) (Scr.IntLiteral True 15 nil) nil)

    let itBinary'' op op' = it ("should transform IR  `" ++ op ++ "` to sCrypt BinaryExpr correctly") $ do
          r1 <- transform2Scrypt $ Just (IR.BinaryExpr op' (LiteralExpr $ IR.BoolLiteral True) (LiteralExpr $ IR.BoolLiteral False))
          r1 `shouldBe` Just (Scr.BinaryExpr (toScryptBinaryOp op') (Scr.BoolLiteral True nil) (Scr.BoolLiteral False nil) nil)

    itBinary "+" IR.Add

    itBinary "-" IR.Sub 

    itBinary "*" IR.Mul 

    itBinary "/" IR.Div

    itBinary "%" IR.Mod

    itBinary' "+=" IR.AddAssign

    itBinary' "-=" IR.SubAssign

    itBinary' "*=" IR.MulAssign

    itBinary' "/=" IR.DivAssign

    itBinary' "%=" IR.ModAssign

    itBinary' "==" IR.Equal 

    itBinary' "!=" IR.Neq 

    itBinary' "<" IR.LessThan 

    itBinary' "<=" IR.LessThanOrEqual 

    itBinary' ">" IR.GreaterThan 

    itBinary' ">=" IR.GreaterThanOrEqual 

    itBinary'' "&&" IR.BoolAnd 

    itBinary'' "||" IR.BoolOr 

  describe "#Ternary" $ do
    itExpr
      "Ternary"
      (IR.TernaryExpr {IR.ternaryCond = LiteralExpr (IR.BoolLiteral True), IR.ternaryTrueBranch = LiteralExpr (IR.IntLiteral {IR.isHex = False, IR.intVal = 1}), IR.ternaryFalseBranch = LiteralExpr (IR.IntLiteral {IR.isHex = False, IR.intVal = 2})})
      (Scr.TernaryExpr (Scr.BoolLiteral True nil) (Scr.IntLiteral False  1 nil) (Scr.IntLiteral False 2 nil) nil)
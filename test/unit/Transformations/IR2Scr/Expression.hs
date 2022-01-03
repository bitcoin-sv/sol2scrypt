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
  let itExpr title e c = it ("should transform IR  `" ++ title ++ "` to sCrypt Type correctly") $ do
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
      (Scr.BytesLiteral [1, 1, 19] nil)
    itExpr
      "BytesLiteral"
      (LiteralExpr $ IR.BytesLiteral [])
      (Scr.BytesLiteral [] nil)

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
    let itBinary op = it ("should transform IR  `" ++ op ++ "` to sCrypt Type correctly") $ do
          r1 <- transform2Scrypt $ Just (IR.BinaryExpr (str2BinaryOp op) (LiteralExpr $ IR.IntLiteral True 15) (LiteralExpr $ IR.IntLiteral True 15))
          r1 `shouldBe` Just (Scr.BinaryExpr (toScryptBinaryOp $ str2BinaryOp op) (Scr.IntLiteral True 15 nil) (Scr.IntLiteral True 15 nil) nil)

    let itBinary' op = it ("should transform IR  `" ++ op ++ "` to sCrypt Type correctly") $ do
          r1 <- transform2Scrypt $ Just (IR.BinaryExpr (str2BinaryOp op) (IdentifierExpr $ IR.Identifier "a") (LiteralExpr $ IR.IntLiteral True 15))
          r1 `shouldBe` Just (Scr.BinaryExpr (toScryptBinaryOp $ str2BinaryOp op) (Scr.Var "a" False nil) (Scr.IntLiteral True 15 nil) nil)

    let itBinary'' op = it ("should transform IR  `" ++ op ++ "` to sCrypt Type correctly") $ do
          r1 <- transform2Scrypt $ Just (IR.BinaryExpr (str2BinaryOp op) (LiteralExpr $ IR.BoolLiteral True) (LiteralExpr $ IR.BoolLiteral False))
          r1 `shouldBe` Just (Scr.BinaryExpr (toScryptBinaryOp $ str2BinaryOp op) (Scr.BoolLiteral True nil) (Scr.BoolLiteral False nil) nil)

    itBinary "+"

    itBinary "-"

    itBinary "*"

    itBinary "/"

    itBinary "%"

    itBinary' "+="

    itBinary' "-="

    itBinary' "*="

    itBinary' "/="

    itBinary' "%="

    itBinary' "=="

    itBinary' "!="

    itBinary' "<"

    itBinary' "<="

    itBinary' ">"

    itBinary' ">="

    itBinary'' "&&"

    itBinary'' "||"

  describe "#Ternary" $ do
    itExpr
      "Ternary"
      (IR.TernaryExpr {IR.ternaryCond = LiteralExpr (IR.BoolLiteral True), IR.ternaryTrueBranch = LiteralExpr (IR.IntLiteral {IR.isHex = False, IR.intVal = 1}), IR.ternaryFalseBranch = LiteralExpr (IR.IntLiteral {IR.isHex = False, IR.intVal = 2})})
      (Scr.TernaryExpr (Scr.BoolLiteral True nil) (Scr.IntLiteral False  1 nil) (Scr.IntLiteral False 2 nil) nil)
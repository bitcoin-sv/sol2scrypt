{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Generables.Expression where

import Scrypt.Generables.Base
import Scrypt.Generables.Expression
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils (nil)

spec :: IO TestTree
spec = testSpec "instance Generable (Expr a)" $ do
  let itcode title e c = it ("should generate sCrypt code for `" ++ title ++ "` correctly") $ do
        e' <- generateScrypt (CodeGenState 0) (Just e)
        e' `shouldBe` c

  describe "#Expr" $ do
    describe "#Literal" $ do
      itcode "BoolLiteral" (Scr.BoolLiteral True nil) "true"

      itcode "IntLiteral" (Scr.IntLiteral True 15 nil) "0xf"
      itcode "IntLiteral" (Scr.IntLiteral False 15 nil) "15"
      itcode "IntLiteral" (Scr.IntLiteral True 32 nil) "0x20"

      itcode "BytesLiteral" (Scr.BytesLiteral False [1, 1, 19] nil) "b'010113'"
      itcode "BytesLiteral" (Scr.BytesLiteral False [] nil) "b''"
      itcode "BytesLiteral" (Scr.BytesLiteral True [240, 159, 152, 138] nil) "\"😊\""
      itcode "BytesLiteral" (Scr.BytesLiteral True [228, 189, 160, 229, 165, 189] nil) "\"你好\""

    describe "#Parens" $ do
      itcode "Parens" (Scr.Parens (Scr.IntLiteral False 0 nil) nil) "(0)"
      itcode "Parens" (Scr.Parens (Scr.BoolLiteral True nil) nil) "(true)"

    describe "#UnaryExpr" $ do
      itcode
        "-"
        (Scr.UnaryExpr Scr.Negate (Scr.IntLiteral False 15 nil) nil)
        "-15"
      itcode
        "-"
        (Scr.UnaryExpr Scr.Negate (Scr.IntLiteral True 15 nil) nil)
        "-(0xf)"
      itcode
        "-"
        (Scr.UnaryExpr Scr.Negate (Scr.Parens (Scr.IntLiteral True 15 nil) nil) nil)
        "-(0xf)"
      itcode
        "++"
        (Scr.UnaryExpr Scr.PreIncrement (Scr.Var "a" False nil) nil)
        "++a"
      itcode
        "++"
        (Scr.UnaryExpr Scr.PostIncrement (Scr.Var "a" False nil) nil)
        "a++"
      itcode
        "++"
        (Scr.UnaryExpr Scr.PreIncrement (Scr.Var "a" True nil) nil)
        "++a"
      itcode
        "--"
        (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False nil) nil)
        "--a"
      itcode
        "--"
        (Scr.UnaryExpr Scr.PostDecrement (Scr.Var "a" False nil) nil)
        "a--"
      itcode
        "!"
        (Scr.UnaryExpr Scr.Not (Scr.Var "a" False nil) nil)
        "!a"

    describe "#BinaryExpr" $ do
      let itBinary title op e1 e2 code = it ("should generate sCrypt code for `" ++ title ++ "` correctly") $ do
            r <- generateScrypt (CodeGenState 0) (Just $ Scr.BinaryExpr op e1 e2 nil)
            r `shouldBe` code

      let itBinaryCode title op = itBinary title op (Scr.IntLiteral False 1 nil) (Scr.IntLiteral False 15 nil) ("1" ++ binaryOp2Str op ++ "15")

      let itBinaryCode' title op = itBinary title op (Scr.Var "a" False nil) (Scr.IntLiteral False 15 nil) ("a" ++ binaryOp2Str op ++ "15")

      itBinaryCode "+" Scr.Add

      itBinaryCode "-" Scr.Sub

      itBinaryCode "*" Scr.Mul

      itBinaryCode "/" Scr.Div

      itBinaryCode "%" Scr.Mod

      itBinaryCode' "+=" Scr.AddAssign

      itBinaryCode' "-=" Scr.SubAssign

      itBinaryCode' "*=" Scr.MulAssign

      itBinaryCode' "/=" Scr.DivAssign

      itBinaryCode' "%=" Scr.ModAssign

      itBinaryCode "==" Scr.Equal

      itBinaryCode "!=" Scr.Neq

      itBinaryCode "<" Scr.LessThan

      itBinaryCode "<=" Scr.LessThanOrEqual

      itBinaryCode ">" Scr.GreaterThan

      itBinaryCode ">=" Scr.GreaterThanOrEqual

      itBinary
        "||"
        Scr.BoolOr
        (Scr.BoolLiteral True nil)
        (Scr.BoolLiteral False nil)
        "true || false"

      itBinary
        "&&"
        Scr.BoolAnd
        (Scr.BoolLiteral True nil)
        (Scr.BoolLiteral False nil)
        "true && false"

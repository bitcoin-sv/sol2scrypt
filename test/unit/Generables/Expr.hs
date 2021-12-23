{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Generables.Expr where

import Scrypt.Generables.Base
import Scrypt.Generables.Expr (binaryOp2Str)
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance Generable (Expr a)" $ do
  let itcode title e c = it ("should generate sCrypt code for `" ++ title ++ "` correctly") $ do
        genCode (Just e) `shouldBe` c

  describe "#Expr" $ do
    describe "#Literal" $ do
      itcode "BoolLiteral" (Scr.BoolLiteral True Nothing) "true"

      itcode "IntLiteral" (Scr.IntLiteral True 15 Nothing) "0xf"
      itcode "IntLiteral" (Scr.IntLiteral False 15 Nothing) "15"
      itcode "IntLiteral" (Scr.IntLiteral True 32 Nothing) "0x20"

      itcode "BytesLiteral" (Scr.BytesLiteral [1, 1, 19] Nothing) "b'010113'"
      itcode "BytesLiteral" (Scr.BytesLiteral [] Nothing) "b''"

    describe "#Parens" $ do
      itcode "Parens" (Scr.Parens (Scr.IntLiteral False 0 Nothing) Nothing) "(0)"
      itcode "Parens" (Scr.Parens (Scr.BoolLiteral True Nothing) Nothing) "(true)"

    describe "#UnaryExpr" $ do
      itcode
        "-"
        (Scr.UnaryExpr Scr.Negate (Scr.IntLiteral False 15 Nothing) Nothing)
        "-15"
      itcode
        "-"
        (Scr.UnaryExpr Scr.Negate (Scr.IntLiteral True 15 Nothing) Nothing)
        "-(0xf)"
      itcode
        "-"
        (Scr.UnaryExpr Scr.Negate (Scr.Parens (Scr.IntLiteral True 15 Nothing) Nothing) Nothing)
        "-(0xf)"
      itcode
        "++"
        (Scr.UnaryExpr Scr.PreIncrement (Scr.Var "a" False Nothing) Nothing)
        "++a"
      itcode
        "++"
        (Scr.UnaryExpr Scr.PostIncrement (Scr.Var "a" False Nothing) Nothing)
        "a++"
      itcode
        "++"
        (Scr.UnaryExpr Scr.PreIncrement (Scr.Var "a" True Nothing) Nothing)
        "++a"
      itcode
        "--"
        (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False Nothing) Nothing)
        "--a"
      itcode
        "--"
        (Scr.UnaryExpr Scr.PostDecrement (Scr.Var "a" False Nothing) Nothing)
        "a--"
      itcode
        "!"
        (Scr.UnaryExpr Scr.Not (Scr.Var "a" False Nothing) Nothing)
        "!a"

    describe "#BinaryExpr" $ do
      let itBinary title op e1 e2 code = it ("should generate sCrypt code for `" ++ title ++ "` correctly") $ do
            genCode (Just $ Scr.BinaryExpr op e1 e2 Nothing) `shouldBe` code

      let itBinaryCode title op = itBinary title op (Scr.IntLiteral False 1 Nothing) (Scr.IntLiteral False 15 Nothing) ("1" ++ binaryOp2Str op ++ "15")

      let itBinaryCode' title op = itBinary title op (Scr.Var "a" False Nothing) (Scr.IntLiteral False 15 Nothing) ("a" ++ binaryOp2Str op ++ "15")

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
        (Scr.BoolLiteral True Nothing)
        (Scr.BoolLiteral False Nothing)
        "true || false"

      itBinary
        "&&"
        Scr.BoolAnd
        (Scr.BoolLiteral True Nothing)
        (Scr.BoolLiteral False Nothing)
        "true && false"

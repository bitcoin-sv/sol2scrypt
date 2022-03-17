{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformations.Sol2IR.Expression where

import IR.Spec as IR
import Test.Tasty
import Test.Tasty.Hspec
import Transformations.Helper

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable Sol.Expression IExpr'" $ do
  let itExpr solidityCode e = it ("should transfrom Solidity `" ++ solidityCode ++ "` to IR Expression correctly") $ do
        ir <- sol2Ir sol2Expr solidityCode
        ir `shouldBe` Just (LiteralExpr e)

  let itExpr' solidityCode e = it ("should transfrom Solidity `" ++ solidityCode ++ "` to IR Expression correctly") $ do
        ir <- sol2Ir sol2Expr solidityCode
        ir `shouldBe` Just e

  let itUnary op solidityCode = it ("should transfrom Solidity `" ++ solidityCode ++ "` to IR Expression correctly") $ do
        r1 <- sol2Ir sol2Expr solidityCode
        r1 `shouldBe` Just (UnaryExpr  op (IR.IdentifierExpr $ IR.Identifier "a"))

  let itParens solidityCode = it ("should transfrom Solidity `" ++ solidityCode ++ "` to IR Expression correctly") $ do
        r1 <- sol2Ir sol2Expr solidityCode
        r1 `shouldBe` Just (ParensExpr (IR.IdentifierExpr $ IR.Identifier "a"))

  let itBinary op solidityCode = it ("should transfrom Solidity `" ++ solidityCode ++ "` to IR Expression correctly") $ do
        r1 <- sol2Ir sol2Expr solidityCode
        r1 `shouldBe` Just (BinaryExpr op (LiteralExpr $ IR.IntLiteral False 1) (LiteralExpr $ IR.IntLiteral False 2))

  let itBinary' op solidityCode = it ("should transfrom Solidity `" ++ solidityCode ++ "` to IR Expression correctly") $ do
        r1 <- sol2Ir sol2Expr solidityCode
        r1 `shouldBe` Just (BinaryExpr op (LiteralExpr $ IR.BoolLiteral False) (LiteralExpr $ IR.BoolLiteral True))

  describe "#PrimaryExpressionBooleanLiteral" $ do
    itExpr "true" (IR.BoolLiteral True)
    itExpr "false" (IR.BoolLiteral False)

  describe "#PrimaryExpressionNumberLiteral" $ do
    itExpr "0x0123abcdef" (IR.IntLiteral True 4893429231)
    itExpr "12345" (IR.IntLiteral False 12345)

  describe "#PrimaryExpressionHexLiteral" $ do
    itExpr "hex\"010113\"" (IR.BytesLiteral [01, 01, 19])

  describe "#Unary" $ do
    itUnary IR.Negate "-a"
    itParens  "(a)"

    itUnary IR.PreIncrement "++a"
    itUnary IR.PostIncrement "a++"
    itUnary IR.PreDecrement "--a"
    itUnary IR.PostDecrement "a--"
    itUnary IR.Not "!a"

  describe "#Binary" $ do
    itBinary IR.Add "1 + 2"
    itBinary IR.Sub "1 - 2"
    itBinary IR.Mul "1 * 2"
    itBinary IR.Div "1 / 2"
    itBinary IR.Mod "1 % 2"
    itBinary IR.AddAssign  "1 += 2"
    itBinary IR.SubAssign  "1 -= 2"
    itBinary IR.DivAssign  "1 /= 2"
    itBinary IR.MulAssign "1 *= 2"
    itBinary IR.ModAssign "1 %= 2"
    itBinary IR.Equal "1 == 2"
    itBinary IR.Neq "1 != 2"
    itBinary IR.LessThan "1 < 2"
    itBinary IR.LessThanOrEqual "1 <= 2"
    itBinary IR.GreaterThan  "1 > 2"
    itBinary IR.GreaterThanOrEqual "1 >= 2"
    itBinary' IR.BoolOr "false || true"
    itBinary' IR.BoolAnd  "false && true"

  describe "#Ternary" $ do
    itExpr' "true? 1: 2" (TernaryExpr {ternaryCond = LiteralExpr (BoolLiteral True), ternaryTrueBranch = LiteralExpr (IntLiteral {isHex = False, intVal = 1}), ternaryFalseBranch = LiteralExpr (IntLiteral {isHex = False, intVal = 2})})
    itExpr' "true? (a + 1): a++" (TernaryExpr {ternaryCond = LiteralExpr (BoolLiteral True), ternaryTrueBranch = IR.ParensExpr {enclosedExpr = BinaryExpr {binaryOp = Add, lExpr = IdentifierExpr (IR.Identifier "a"), rExpr = LiteralExpr (IntLiteral {isHex = False, intVal = 1})}}, ternaryFalseBranch = UnaryExpr {unaryOp = PostIncrement, uExpr = IdentifierExpr (IR.Identifier "a")}})

  describe "#MemberAccess" $ do
    itExpr' "a.b" (IR.MemberAccessExpr {instanceExpr = IdentifierExpr (IR.Identifier "a"), member = IR.Identifier "b"})
    itExpr' "a.b.c" (IR.MemberAccessExpr {instanceExpr = IR.MemberAccessExpr {instanceExpr = IdentifierExpr (IR.Identifier "a"), member = IR.Identifier "b"}, member = IR.Identifier "c"})
    itExpr' "msg.sender" (IR.IdentifierExpr (IR.ReservedId "msgSender"))
    itExpr' "msg.value" (IR.IdentifierExpr (IR.ReservedId "msgValue"))
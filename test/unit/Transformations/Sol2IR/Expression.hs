{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformations.Sol2IR.Expression where

import IR.Spec as IR
import IR.Transformer
import Solidity.Spec as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transformations.Helper

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable Sol.Expression IExpr'" $ do

  let itExpr solidityCode e = it ("should transfrom Solidity `" ++ solidityCode ++ "` to IR Expression correctly") $ do
        ir <- sol2Ir sol2Expr solidityCode
        ir `shouldBe` Just (LiteralExpr e)

  let itUnary op solidityCode = it ("should transfrom Solidity `" ++ solidityCode ++ "` to IR Expression correctly") $ do
        r1 <- sol2Ir sol2Expr solidityCode
        r1 `shouldBe` transformUnaryExpr op (Just (IR.IdentifierExpr $ IR.Identifier "a"))

  let itBinary op solidityCode = it ("should transfrom Solidity `" ++ solidityCode ++ "` to IR Expression correctly") $ do
        r1 <- sol2Ir sol2Expr solidityCode
        r1 `shouldBe` Just (BinaryExpr (str2BinaryOp op) (LiteralExpr $ IR.IntLiteral False 1) (LiteralExpr $ IR.IntLiteral False 2))

  let itBinary' op solidityCode = it ("should transfrom Solidity `" ++ solidityCode ++ "` to IR Expression correctly") $ do
        r1 <- sol2Ir sol2Expr solidityCode
        r1 `shouldBe` Just (BinaryExpr (str2BinaryOp op) (LiteralExpr $ IR.BoolLiteral False) (LiteralExpr $ IR.BoolLiteral True))

  describe "#PrimaryExpressionBooleanLiteral" $ do
    itExpr "true" (IR.BoolLiteral True)
    itExpr "false" (IR.BoolLiteral False)

    itExpr "0x0123abcdef" (IR.IntLiteral True 4893429231)
    itExpr "12345" (IR.IntLiteral False 12345)

    itExpr "hex\"010113\"" (IR.BytesLiteral [01, 01, 19])

    itUnary "-" "-a"
    itUnary "()" "(a)"

    itUnary "++" "++a"
    itUnary "()++" "a++"
    itUnary "--" "--a"
    itUnary "()--" "a--"
    itUnary "!" "!a"

    itBinary "+" "1 + 2"
    itBinary "-" "1 - 2"
    itBinary "*" "1 * 2"
    itBinary "/" "1 / 2"
    itBinary "%" "1 % 2"
    itBinary "+=" "1 += 2"
    itBinary "-=" "1 -= 2"
    itBinary "/=" "1 /= 2"
    itBinary "*=" "1 *= 2"
    itBinary "%=" "1 %= 2"
    itBinary "==" "1 == 2"
    itBinary "!=" "1 != 2"
    itBinary "<" "1 < 2"
    itBinary "<=" "1 <= 2"
    itBinary ">" "1 > 2"
    itBinary ">=" "1 >= 2"
    itBinary' "||" "false || true"
    itBinary' "&&" "false && true"
{-# LANGUAGE ScopedTypeVariables #-}
module Transformations.Sol2IR.Expression where

import IR.Spec as IR
import IR.Transformer
import Solidity.Spec as Sol
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable Sol.Expression IExpr'" $ do
  let itExpr title solidityCode e = it ("should transfrom Solidity `" ++ title ++ "` to IR Expression correctly") $ do
        expr :: Sol.Expression <- parseIO solidityCode
        r1 <- transform2IR (TransformState []) expr
        r1 `shouldBe` Just (LiteralExpr e)

  let itUnary op = it ("should transfrom Solidity `" ++ op ++ "` to IR Expression correctly") $ do
        r1 <- transform2IR (TransformState []) (Unary op (Literal (PrimaryExpressionIdentifier (Sol.Identifier "a"))))
        r1 `shouldBe` transformUnaryExpr op (Just (IR.IdentifierExpr $ IR.Identifier "a"))

  let itBinary op = it ("should transfrom Solidity `" ++ op ++ "` to IR Expression correctly") $ do
        r1 <- transform2IR (TransformState []) (Binary op (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "2" Nothing))))
        r1 `shouldBe` Just (BinaryExpr (str2BinaryOp op) (LiteralExpr $ IR.IntLiteral False 1) (LiteralExpr $ IR.IntLiteral False 2))

  describe "#PrimaryExpressionBooleanLiteral" $ do
    itExpr
      "BoolLiteral"
      "true"
      (IR.BoolLiteral True)
    itExpr
      "BoolLiteral"
      "false"
      (IR.BoolLiteral False)

    itExpr
      "NumberLiteralHex"
      "0x0123abcdef"
      (IR.IntLiteral True 4893429231)
    itExpr
      "NumberLiteralDec"
      "12345"
      (IR.IntLiteral False 12345)

    itExpr
      "HexLiteral"
      "hex\"010113\""
      (IR.BytesLiteral [01, 01, 19])

    itUnary "-"
    itUnary "()"
    itUnary "++"

    itUnary "()++"

    itUnary "--"

    itUnary "()--"

    itUnary "!"

    itBinary "+"

    itBinary "-"

    itBinary "*"

    itBinary "/"

    itBinary "%"

    itBinary "+="

    itBinary "-="

    itBinary "*="

    itBinary "/="

    itBinary "%="

    itBinary "=="

    itBinary "!="

    itBinary "<"

    itBinary "<="

    itBinary ">"

    itBinary ">="

    itBinary "&&"

    itBinary "||"

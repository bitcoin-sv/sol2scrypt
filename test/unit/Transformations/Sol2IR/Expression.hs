module Transformations.Sol2IR.Expression where

import IR.Spec as IR
import IR.Transformer
import Solidity.Spec as Sol
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable Sol.Expression IExpr'" $ do
  let itExpr title e1 e2 = it ("should transfrom Solidity `" ++ title ++ "` to IR Expression correctly") $ do
        r1 <- transform2IR TransformState (Literal e1)
        r1 `shouldBe` Just (LiteralExpr e2)

  let itUnary op = it ("should transfrom Solidity `" ++ op ++ "` to IR Expression correctly") $ do
        r1 <- transform2IR TransformState (Unary op (Literal (PrimaryExpressionIdentifier (Sol.Identifier "a"))))
        r1 `shouldBe` transformUnaryExpr op (Just (IR.IdentifierExpr $ IR.Identifier "a"))

  let itBinary op = it ("should transfrom Solidity `" ++ op ++ "` to IR Expression correctly") $ do
        r1 <- transform2IR TransformState (Binary op (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "2" Nothing))))
        r1 `shouldBe` Just (BinaryExpr (str2BinaryOp op) (LiteralExpr $ IR.IntLiteral False 1) (LiteralExpr $ IR.IntLiteral False 2))

  describe "#PrimaryExpressionBooleanLiteral" $ do
    itExpr
      "BoolLiteral"
      (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral "true"))
      (IR.BoolLiteral True)
    itExpr
      "BoolLiteral"
      (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral "false"))
      (IR.BoolLiteral False)

    itExpr
      "NumberLiteralHex"
      (PrimaryExpressionNumberLiteral (NumberLiteralHex "0123abcdef" Nothing))
      (IR.IntLiteral True 4893429231)
    itExpr
      "NumberLiteralDec"
      (PrimaryExpressionNumberLiteral (NumberLiteralDec "12345" Nothing))
      (IR.IntLiteral False 12345)

    itExpr
      "HexLiteral"
      (PrimaryExpressionHexLiteral (HexLiteral "010113"))
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

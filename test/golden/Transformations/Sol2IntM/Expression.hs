module Transformations.Sol2IntM.Expression where

import Solidity.Spec as Sol
import IR.Spec as IR
import IR.Transformer
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable Sol.Expression IExpr'" $ do
  it "should transfrom Solidity `BoolLiteral` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral "true")))
    r1 `shouldBe` Just (LiteralExpr $ IR.BoolLiteral True)
    r2 <- transform2IR TransformState (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral "false")))
    r2 `shouldBe` Just (LiteralExpr $ IR.BoolLiteral False)

  it "should transfrom Solidity `NumberLiteral` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Literal (PrimaryExpressionNumberLiteral (NumberLiteralHex "0123abcdef" Nothing)))
    r1 `shouldBe` Just (LiteralExpr $ IR.IntLiteral True 4893429231)
    r2 <- transform2IR TransformState (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "12345" Nothing)))
    r2 `shouldBe` Just (LiteralExpr $ IR.IntLiteral False 12345)
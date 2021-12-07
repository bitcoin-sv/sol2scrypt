module Transformations.Sol2IntM.Expression where

import Solidity.Spec as Sol
import Intermediate.Spec as IntM
import Intermediate.Transformer
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToIntermediateTransformable Sol.Expression IExpr'" $ do
  it "should transfrom Solidity `BoolLiteral` to Intermediate Expression correctly" $ do
    r1 <- transform2Intermediate TransformState (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral "true")))
    r1 `shouldBe` Just (IntM.BoolLiteral True)
    r2 <- transform2Intermediate TransformState (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral "false")))
    r2 `shouldBe` Just (IntM.BoolLiteral False)

  it "should transfrom Solidity `NumberLiteral` to Intermediate Expression correctly" $ do
    r1 <- transform2Intermediate TransformState (Literal (PrimaryExpressionNumberLiteral (NumberLiteralHex "0123abcdef" Nothing)))
    r1 `shouldBe` Just (IntM.IntLiteral True 4893429231)
    r2 <- transform2Intermediate TransformState (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "12345" Nothing)))
    r2 `shouldBe` Just (IntM.IntLiteral False 12345)
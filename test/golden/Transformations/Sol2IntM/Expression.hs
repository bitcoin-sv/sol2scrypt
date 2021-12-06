module Transformations.Sol2IntM.Expression where

import Intermediate.Spec as IntM
import Intermediate.Transformer
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "Transformations.Sol2IntM.Expression" $ do
  describe "#transformSolExpression" $ do
    it "should transfrom Solidity `BoolLiteral` to Intermediate Expression correctly" $ do
      r1 <- transformSolExpression "true"
      r1 `shouldBe` Just (IntM.BoolLiteral True)
      r2 <- transformSolExpression "false"
      r2 `shouldBe` Just (IntM.BoolLiteral False)

    it "should transfrom Solidity `NumberLiteral` to Intermediate Expression correctly" $ do
      r1 <- transformSolExpression "0x0123abcdef"
      r1 `shouldBe` Just (IntM.IntLiteral True 4893429231)
      r2 <- transformSolExpression "12345"
      r2 `shouldBe` Just (IntM.IntLiteral False 12345)
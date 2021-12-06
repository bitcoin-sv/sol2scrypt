module Transformations.Sol2IntM.Type where

import Intermediate.Spec as IntM
import Intermediate.Transformer
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "Transformations.Sol2IntM.Type" $ do
  describe "#transformSolTypeName" $ do
    it "should transfrom Solidity `BoolType` to Intermediate Type correctly" $ do
      r <- transformSolTypeName "bool"
      r `shouldBe` Just ITypeBool

    it "should transfrom Solidity `IntType` to Intermediate Type correctly" $ do
      r1 <- transformSolTypeName "int"
      r1 `shouldBe` Just ITypeInt
      r2 <- transformSolTypeName "int256"
      r2 `shouldBe` Just ITypeInt
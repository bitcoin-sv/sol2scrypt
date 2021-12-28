module Transformations.Sol2IR.Variable where

import IR.Spec as IR
import IR.Transformer
import Solidity.Spec as Sol
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "Variables" $ do
  describe "#Parameter" $ do
    it "should transfrom Solidity `Parameter` to IR IParam' correctly" $ do
      r <- transform2IR (TransformState []) (Parameter (TypeNameElementaryTypeName BoolType) Nothing (Just (Sol.Identifier "p")))
      r `shouldBe` Just (IR.Param (ElementaryType Bool) (IR.Identifier "p"))

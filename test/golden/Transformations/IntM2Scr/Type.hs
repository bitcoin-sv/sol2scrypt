module Transformations.IntM2Scr.Type where

import Intermediate.Spec as IntM
import Intermediate.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "Transformations.IntM2Scr.Type" $ do
  describe "#transformIntermediateType" $ do
    it "should transform Intermediate `ITypeBool` to sCrypt Type correctly" $ do
      transformIntermediateType ITypeBool `shouldBe` Scr.Bool

    it "should transform Intermediate `ITypeInt` to sCrypt Type correctly" $ do
      transformIntermediateType ITypeInt `shouldBe` Scr.Int

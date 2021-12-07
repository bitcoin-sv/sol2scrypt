module Transformations.IntM2Scr.Type where

import Intermediate.Spec as IntM
import Intermediate.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToScryptTransformable IType Type" $ do
  it "should transform Intermediate `ITypeBool` to sCrypt Type correctly" $ do
    r <- transform2Scrypt (Just ITypeBool)
    r `shouldBe` Just Scr.Bool

  it "should transform Intermediate `ITypeInt` to sCrypt Type correctly" $ do
    r <- transform2Scrypt (Just ITypeInt)
    r `shouldBe` Just Scr.Int

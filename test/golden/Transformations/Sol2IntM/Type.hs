module Transformations.Sol2IntM.Type where

import Intermediate.Spec as IntM
import Intermediate.Transformer
import Solidity.Spec
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToIntermediateTransformable TypeName IType'" $ do
  it "should transfrom Solidity `BoolType` to Intermediate Type correctly" $ do
    r <- transform2Intermediate TransformState (TypeNameElementaryTypeName BoolType)
    r `shouldBe` Just ITypeBool

  it "should transfrom Solidity `IntType` to Intermediate Type correctly" $ do
    r1 <- transform2Intermediate TransformState (TypeNameElementaryTypeName (IntType $ Just 8))
    r1 `shouldBe` Just ITypeInt
    r2 <- transform2Intermediate TransformState (TypeNameElementaryTypeName (UintType $ Just 256))
    r2 `shouldBe` Just ITypeInt
module Transformations.Sol2IR.Type where

import IR.Spec as IR
import IR.Transformer
import Solidity.Spec
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable TypeName IType'" $ do
  it "should transfrom Solidity `BoolType` to IR Type correctly" $ do
    r <- transform2IR TransformState (TypeNameElementaryTypeName BoolType)
    r `shouldBe` (Just $ ElementaryType Bool)

  it "should transfrom Solidity `IntType` to IR Type correctly" $ do
    r1 <- transform2IR TransformState (TypeNameElementaryTypeName (IntType $ Just 8))
    r1 `shouldBe` (Just $ ElementaryType Int)
    r2 <- transform2IR TransformState (TypeNameElementaryTypeName (UintType $ Just 256))
    r2 `shouldBe` (Just $ ElementaryType Int)
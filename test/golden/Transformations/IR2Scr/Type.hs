module Transformations.IR2Scr.Type where

import IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToScryptTransformable IType Type" $ do
  it "should transform IR `Bool` to sCrypt Type correctly" $ do
    r <- transform2Scrypt (Just $ ElementaryType IR.Bool)
    r `shouldBe` Just Scr.Bool

  it "should transform IR `Int` to sCrypt Type correctly" $ do
    r <- transform2Scrypt (Just $ ElementaryType IR.Int)
    r `shouldBe` Just Scr.Int

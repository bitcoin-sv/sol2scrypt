module Transformations.IntM2Scr.Type where

import Intermediate.Spec as IntM
import Intermediate.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToScryptTransformable IType Type" $ do
  it "should transform Intermediate `Bool` to sCrypt Type correctly" $ do
    r <- transform2Scrypt (Just $ ElementaryType IntM.Bool)
    r `shouldBe` Just Scr.Bool

  it "should transform Intermediate `Int` to sCrypt Type correctly" $ do
    r <- transform2Scrypt (Just $ ElementaryType IntM.Int)
    r `shouldBe` Just Scr.Int

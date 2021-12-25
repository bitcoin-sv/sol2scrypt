module Transformations.IR2Scr.Variable where

import IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils

spec :: IO TestTree
spec = testSpec "Variables" $ do
  describe "instance ToScryptTransformable IParam (Scr.Param Ann)" $ do
    it "should transform IR `Param` to sCrypt Type correctly" $ do
      r <- transform2Scrypt (IR.Param (ElementaryType IR.Bool) (IR.Identifier "p"))
      r `shouldBe` Scr.Param (TypeAnn Scr.Bool nil) (NameAnn "p" nil) (Const False) Nothing Scr.Default (IsStateProp False) nil

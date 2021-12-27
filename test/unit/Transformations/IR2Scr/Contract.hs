module Transformations.IR2Scr.Contract where

import IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils

spec :: IO TestTree
spec = testSpec "Contract" $ do
  describe "instance ToScryptTransformable Contract" $ do
    it "should transform IR `IContractBodyElement` to sCrypt Type correctly" $ do
      r <- transform2Scrypt (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (IR.ElementaryType IR.Int) IR.Public Nothing))
      r `shouldBe` Scr.Param (TypeAnn Scr.Int nil) (NameAnn "a" nil) (Const False) Nothing Scr.Public (IsStateProp True) nil

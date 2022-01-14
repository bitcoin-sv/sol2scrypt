module Transformations.IR2Scr.Contract where

import IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils

spec :: IO TestTree
spec = testSpec "Contract" $ do
  describe "instance ToScryptTransformable IContractBodyElement" $ do
    it "should transform IR `StateVariableDeclaration` to sCrypt Param correctly" $ do
      r <- transform2Scrypt (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (IR.ElementaryType IR.Int) IR.Public Nothing False))
      r `shouldBe` Scr.Param (TypeAnn Scr.Int nil) (NameAnn "a" nil) (Const False) Nothing Scr.Public (IsStateProp True) nil


    it "should transform IR `StateVariableDeclaration` to sCrypt Static correctly" $ do
      r <- transform2Scrypt (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (IR.ElementaryType IR.Int) IR.Public (Just (LiteralExpr (IR.IntLiteral {IR.isHex = False, IR.intVal = 8}))) True))
      r `shouldBe` Scr.Static (Scr.Param (TypeAnn Scr.Int nil) (NameAnn "a" nil) (Const True) Nothing Scr.Public (IsStateProp False) nil) (Scr.IntLiteral False 8 nil) nil

    it "should transform IR `ConstructorDefinition` to sCrypt Constructor correctly" $ do
      r <- transform2Scrypt (IR.ConstructorDefinition (IR.Constructor (IR.ParamList [IR.Param (ElementaryType IR.Bool) (IR.Identifier "a")]) (IR.Block [])))
      r `shouldBe`  Just ( Scr.Constructor [Scr.Param (TypeAnn Scr.Bool nil) (NameAnn "a" nil) (Const False) Nothing Scr.Default (IsStateProp False) nil] (Scr.CtorBody [] nil) nil)

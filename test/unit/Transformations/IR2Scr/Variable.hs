{-# LANGUAGE FlexibleContexts #-}

module Transformations.IR2Scr.Variable where

import IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils


spec :: IO TestTree
spec = testSpec "Variables" $ do
  let itParam title e1 e2 = it ("should transfrom IR `" ++ title ++ "` to sCrypt NameAnn correctly") $ do
        r <- transform2Scrypt $ Just $ IR.Param e1 (IR.Identifier "a")
        r `shouldBe` Just e2

  let itStateVariable title e1 e2 = it ("should transfrom IR `" ++ title ++ "` to sCrypt Param correctly") $ do
        r <- transform2Scrypt e1
        r `shouldBe` e2

  describe "instance ToScryptTransformable IParam (Scr.Param Ann)" $ do
    itParam
      "bool param"
      (ElementaryType IR.Bool)
      (Scr.Param (TypeAnn Scr.Bool nil) (NameAnn "a" nil) (Const False) Nothing Scr.Default (IsStateProp False) nil)
    itParam
      "int param"
      (ElementaryType IR.Int)
      (Scr.Param (TypeAnn Scr.Int nil) (NameAnn "a" nil) (Const False) Nothing Scr.Default (IsStateProp False) nil)
    itParam
      "Bytes param"
      (ElementaryType IR.Bytes)
      (Scr.Param (TypeAnn Scr.Bytes nil) (NameAnn "a" nil) (Const False) Nothing Scr.Default (IsStateProp False) nil)

  describe "instance ToScryptTransformable IStateVariable (Scr.Param Ann)" $ do
    itStateVariable
      "bool a;"
      (IR.StateVariable (IR.Identifier "a") (ElementaryType IR.Bool) IR.Default Nothing False)
      (Scr.Param (TypeAnn Scr.Bool nil) (NameAnn "a" nil) (Const False) Nothing Scr.Default (IsStateProp True) nil)

    itStateVariable
      "int a;"
      (IR.StateVariable (IR.Identifier "a") (ElementaryType IR.Int) IR.Default Nothing False)
      (Scr.Param (TypeAnn Scr.Int nil) (NameAnn "a" nil) (Const False) Nothing Scr.Default (IsStateProp True) nil)


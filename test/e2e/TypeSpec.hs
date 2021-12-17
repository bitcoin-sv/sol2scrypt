{-# LANGUAGE ScopedTypeVariables #-}

module TypeSpec where

import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transpiler

spec :: IO TestTree
spec = testSpec "Transpile Type" $ do
  describe "#ElementaryTypeName" $ do
    describe "#BoolType" $ do
      it "should transpile Solidity `bool` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bool"
        scryptCode tr `shouldBe` "bool"

    describe "#UintType" $ do
      it "should transpile Solidity `uint8` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint8"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint256` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint256"
        scryptCode tr `shouldBe` "int"

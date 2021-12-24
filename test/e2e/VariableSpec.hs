{-# LANGUAGE ScopedTypeVariables #-}

module VariableSpec where

import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transpiler
import Utils

spec :: IO TestTree
spec = testSpec "Transpile Variable" $ do
  describe "#Parameter" $ do
    it "should transpile Solidity `Parameter` correctly" $ do
      tr :: TranspileResult Parameter IParam' (Maybe (Scr.Param Ann)) <- transpile "uint256 a"
      scryptCode tr `shouldBe` "int a"
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
  let itProperty sol scrypt = it ("should transpile Solidity `" ++ sol ++ "` correctly") $ do
        tr :: TranspileResult ContractPart IContractBodyElement' (Maybe (Scr.Param Ann)) <- transpile sol
        scryptCode tr `shouldBe` scrypt

  describe "#Property" $ do
      itProperty "uint storedData;" "@state int storedData;"
      itProperty "int storedData;" "@state int storedData;"
      itProperty "bool a;" "@state bool a;"
      itProperty "bytes a;" "@state bytes a;"
      itProperty "bytes private a;" "@state private bytes a;"
      itProperty "bytes public a;" "@state public bytes a;"

  describe "#Parameter" $ do
    it "should transpile Solidity `Parameter` correctly" $ do
      tr :: TranspileResult Parameter IParam' (Maybe (Scr.Param Ann)) <- transpile "uint256 a"
      scryptCode tr `shouldBe` "int a"
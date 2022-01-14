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
  let itParameter sol scrypt = it ("should transpile Solidity `" ++ sol ++ "` correctly") $ do
        tr :: TranspileResult Parameter IParam' (Maybe (Scr.Param Ann)) <- transpile sol
        scryptCode tr `shouldBe` scrypt
  let itStatic sol scrypt = it ("should transpile Solidity `" ++ sol ++ "` correctly") $ do
            tr :: TranspileResult ContractPart IContractBodyElement' (Maybe (Scr.Static Ann)) <- transpile sol
            scryptCode tr `shouldBe` scrypt
  describe "#Property" $ do
      itProperty "uint storedData;" "\n@state\nint storedData;"
      itProperty "int storedData;" "\n@state\nint storedData;"
      itProperty "bool a;" "\n@state\nbool a;"
      itProperty "bytes a;" "\n@state\nbytes a;"
      itProperty "bytes private a;" "\n@state\nprivate bytes a;"
      itProperty "bytes public a;" "\n@state\npublic bytes a;"


        
  describe "#Parameter" $ do
    itParameter "int a" "int a"
    itParameter "uint a" "int a"
    itParameter "byte a" "bytes a"
    itParameter "bytes a" "bytes a"
    itParameter "address a" "PubKeyHash a"
    itParameter "bool a" "bool a"

  describe "#Static" $ do
    itStatic "uint constant x = 1;" "\nstatic const int x = 1;"
    itStatic "uint constant x = 1 + 1 *(1-1);" "\nstatic const int x = 1 + 1 * (1 - 1);"
    itStatic "bool constant x = true;" "\nstatic const bool x = true;"
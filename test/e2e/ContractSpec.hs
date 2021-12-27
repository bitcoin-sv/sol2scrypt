{-# LANGUAGE ScopedTypeVariables #-}

module ContractSpec where

import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transpiler
import Utils

spec :: IO TestTree
spec = testSpec "Transpile Contract" $ do
  let itTransContract sol scrypt = it "should transpile Solidity contract correctly" $ do
        tr :: TranspileResult ContractDefinition IContract' (Maybe (Scr.Contract Ann)) <- transpile sol
        scryptCode tr `shouldBe` scrypt

  describe "#Contract" $ do
    itTransContract
     "contract A { uint a; function set(uint x) external { a = x; } }"
     "contract A { @state int a; public function set(int x, SigHashPreimage txPreimage) { this.a = x; require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); } }"
  


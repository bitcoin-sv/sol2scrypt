{-# LANGUAGE ScopedTypeVariables #-}

module FunctionSpec where

import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transpiler
import Utils

spec :: IO TestTree
spec = testSpec "Transpile Function" $ do
  let itTranspile title sol scr = it ("should transpile Solidity " ++ title ++ " correctly") $ do
        tr :: TranspileResult Sol.ContractPart IFunction' (Maybe (Scr.Function Ann)) <- transpile sol
        scryptCode tr `shouldBe` scr

  itTranspile
    "external pure function without return"
    "function set(uint x) external pure { uint y = x; }"
    "public function set(int x) { int y = x; require(true); }"

  itTranspile
    "external pure function with named return"
    "function set(uint x) external pure returns (uint y) { y = x; }"
    "public function set(int x, int _y) { int y = 0; y = x; require(y == _y); }"

  itTranspile
    "external pure function with unnamed return"
    "function set(uint x) external pure returns (uint) { y = x; return y; }"
    "public function set(int x, int retVal) { y = x; require(y == retVal); }"

  itTranspile
    "external function without return"
    "function set(uint x) external { storedData = x; }"
    "public function set(int x, SigHashPreimage txPreimage) { storedData = x; require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); }"

  itTranspile
    "external view function with named return"
    "function set(uint x) external view returns (uint y) { y = x; }"
    "public function set(int x, SigHashPreimage txPreimage, int _y) { int y = 0; y = x; require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); require(y == _y); }"

  itTranspile
    "public view function without return"
    "function get() public view { storedData = 1; }"
    "function get() : bool { storedData = 1; return true; }"
  
  itTranspile
    "public view function with named return"
    "function get() public view returns (uint y) { y = storedData; }"
    "function get() : int { int y = 0; y = storedData; return y; }"

  itTranspile
    "public view function with unnamed return"
    "function get() public view returns (uint) { return storedData; }"
    "function get() : int { return storedData; }"
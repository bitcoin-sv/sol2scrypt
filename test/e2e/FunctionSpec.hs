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


  describe "#public " $ do

    itTranspile
      "public function"
      "function get() public { return x; }"
      "function get() : bool { return x; }"

    itTranspile
      "pure function"
      "function get() public pure { return x; }"
      "function get() : bool { return x; }"

    itTranspile
      "constant function"
      "function get() public constant { return x; }"
      "function get() : bool { return x; }"

    itTranspile
      "view function"
      "function get() public view { return x; }"
      "function get() : bool { return x; }"

    itTranspile
      "payable function"
      "function get() public payable { return x; }"
      "function get() : bool { return x; }"

    itTranspile
      "public function with returns"
      "function privateFunc() public pure returns (bool memory) { return true; }"
      "function privateFunc() : bool { return true; }"

  describe "#private " $ do

    itTranspile
      "private function"
      "function get() private { return x; }"
      "private function get() : bool { return x; }"

    itTranspile
      "pure function"
      "function get() private pure { return x; }"
      "private function get() : bool { return x; }"

    itTranspile
      "constant function"
      "function get() private constant { return x; }"
      "private function get() : bool { return x; }"

    itTranspile
      "view function"
      "function get() private view { return x; }"
      "private function get() : bool { return x; }"

    itTranspile
      "payable function"
      "function get() private payable { return x; }"
      "private function get() : bool { return x; }"

    itTranspile
      "private function with returns"
      "function privateFunc() private pure returns (bool memory) { return true; }"
      "private function privateFunc() : bool { return true; }"

  describe "#internal " $ do

    itTranspile
      "internal function"
      "function get() internal { return x; }"
      "private function get() : bool { return x; }"

    itTranspile
      "pure function"
      "function get() internal pure { return x; }"
      "private function get() : bool { return x; }"

    itTranspile
      "constant function"
      "function get() internal constant { return x; }"
      "private function get() : bool { return x; }"

    itTranspile
      "view function"
      "function get() internal view { return x; }"
      "private function get() : bool { return x; }"

    itTranspile
      "payable function"
      "function get() internal payable { return x; }"
      "private function get() : bool { return x; }"

    itTranspile
      "internal function with returns"
      "function privateFunc() internal pure returns (bool memory) { return true; }"
      "private function privateFunc() : bool { return true; }"

  describe "#external " $ do

    itTranspile
      "get function"
      "function get() external payable { return x; }"
      "public function get(SigHashPreimage txPreimage) { require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); require(x == retVal); }"

    itTranspile
      "pure get function"
      "function get() external pure { return x; }"
      "public function get() { require(x == retVal); }"

    itTranspile
      "constant get function"
      "function get() external constant { return x; }"
      "public function get(SigHashPreimage txPreimage) { require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); require(x == retVal); }"

    itTranspile
      "view get function"
      "function get() external view { return x; }"
      "public function get(SigHashPreimage txPreimage) { require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); require(x == retVal); }"

    itTranspile
      "payable get function"
      "function get() external payable { return x; }"
      "public function get(SigHashPreimage txPreimage) { require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); require(x == retVal); }"

    itTranspile
      "set function"
      "function set(uint x) external { storedData = x; }"
      "public function set(int x, SigHashPreimage txPreimage) { storedData = x; require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); }"

    itTranspile
      "constant set function"
      "function set(uint x) external constant { storedData = x; }"
      "public function set(int x, SigHashPreimage txPreimage) { storedData = x; require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); }"

    itTranspile
      "pure set function"
      "function set(uint x) external pure { storedData = x; }"
      "public function set(int x) { storedData = x; require(true); }"

    itTranspile
      "view set function"
      "function set(uint x) external view { storedData = x; }"
      "public function set(int x, SigHashPreimage txPreimage) { storedData = x; require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); }"

    itTranspile
      "payable set function"
      "function set(uint x) external payable { storedData = x; }"
      "public function set(int x, SigHashPreimage txPreimage) { storedData = x; require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); }"

    itTranspile
      "public view function with returns"
      "function get() public view returns (uint) { return storedData; }"
      "function get() : int { return storedData; }"

    itTranspile
      "external function with returns"
      "function get() external view returns (uint) { return storedData; }"
      "public function get(SigHashPreimage txPreimage, int retVal) { require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); require(storedData == retVal); }"


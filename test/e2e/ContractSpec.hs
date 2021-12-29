{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ContractSpec where

import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Text.RawString.QQ
import Transpiler
import Utils

spec :: IO TestTree
spec = testSpec "Transpile Contract" $ do
  let itTransContract sol scrypt = it "should transpile Solidity contract correctly" $ do
        tr :: TranspileResult ContractDefinition IContract' (Maybe (Scr.Contract Ann)) <- transpile sol
        scryptCode tr `shouldBe` scrypt

  itTransContract
    [r|contract A {
    uint a;

    function set(uint x) external {
        a = x;
    }
}|]
    "contract A { @state int a; public function set(int x, SigHashPreimage txPreimage) { this.a = x; require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); } }"

  itTransContract
    [r|contract SimpleStorage {
    uint storedData;

    function set(uint x) external {
        storedData = x;
    }

    function get() public view returns (uint) {
        return storedData;
    }
}|]
    "contract SimpleStorage { @state int storedData; public function set(int x, SigHashPreimage txPreimage) { this.storedData = x; require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); } function get() : int { return this.storedData; } }"

  itTransContract
    [r|contract A {
    uint a;

    function set(uint x) public {
        a = x;
        {
          uint a = 2;
          a = x;
          x = a;
        }
      a = 3;
      x = a; 
    }

    function add(uint a) {
        a = 1; 
        uint b = a;
    }
}|]
    "contract A { @state int a; function set(int x) : bool { this.a = x; {int a = 2; a = x; x = a;} this.a = 3; x = this.a; return true; } function add(int a) : bool { a = 1; int b = a; return true; } }"

  itTransContract
    [r|contract flipper {
    bool private value;

    function flip() public {
        value = !value;
    }

    function get() public view returns (bool) {
        return value;
    }
}|]
    "contract flipper { @state private bool value; function flip() : bool { this.value = !this.value; return true; } function get() : bool { return this.value; } }"

  itTransContract
    [r|contract flipper {
    bool private value;

    function flip() external {
        value = !value;
    }

    function get() public view returns (bool) {
        return value;
    }
}|]
    "contract flipper { @state private bool value; public function flip(SigHashPreimage txPreimage) { this.value = !this.value; require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); } function get() : bool { return this.value; } }"

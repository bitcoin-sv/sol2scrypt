{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProgramSpec where

import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Text.RawString.QQ
import Transpiler
import Utils

spec :: IO TestTree
spec = testSpec "Transpile Program" $ do
  let itProgram title sol scrypt = it ("should transpile Solidity `" ++ title ++ "` correctly") $ do
        tr :: TranspileResult SolidityCode IProgram' (Maybe (Scr.Program Ann)) <- transpile sol
        scryptCode tr `shouldBe` scrypt


  itProgram "Program only with a contract "
      [r|
      
contract SimpleStorage {
    uint storedData;

    constructor() {}
}|]
      [r|contract SimpleStorage {
  @state
  int storedData;

  constructor() {
  }
}|]

  itProgram "Program with pragma"
      [r|
      
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract flipper {
    bool private value;

    /// Constructor that initializes the `bool` value to the given `init_value`.
    constructor(bool initvalue) {
        value = initvalue;
    }

    /// A message that can be called on instantiated contracts.
    /// This one flips the value of the stored `bool` from `true`
    /// to `false` and vice versa.
    function flip() public {
        value = !value;
    }

    /// Simply returns the current value of our `bool`.
    function get() public view returns (bool) {
        return value;
    }
}
|]
      [r|contract flipper {
  @state
  private bool value;

  constructor(bool initvalue) {
    this.value = initvalue;
  }

  function flip() : bool {
    this.value = !this.value;
    return true;
  }

  function get() : bool {
    return this.value;
  }
}|]



  itProgram "Program with multiple contracts "
      [r|
      
contract SimpleStorage {
    uint storedData;

    constructor() {
        storedData = 0;
    }
}


contract flipper {
    bool private value;

    function flip() public {
        value = !value;
    }

    function get() public view returns (bool) {
        return value;
    }
}

contract A {
    uint a;

    function set(uint x) external {
        a = x;
    }
}

|]
      [r|contract SimpleStorage {
  @state
  int storedData;

  constructor() {
    this.storedData = 0;
  }
}

contract flipper {
  @state
  private bool value;

  function flip() : bool {
    this.value = !this.value;
    return true;
  }

  function get() : bool {
    return this.value;
  }
}

contract A {
  @state
  int a;

  public function set(int x, SigHashPreimage txPreimage) {
    this.a = x;
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    require(hash256(output) == SigHash.hashOutputs(txPreimage));
  }
}|]



  itProgram "Program with multiple contracts and with pragma "
      [r|
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract SimpleStorage {
    uint storedData;

    constructor() {
        storedData = 0;
    }
}


contract flipper {
    bool private value;

    function flip() public {
        value = !value;
    }

    function get() public view returns (bool) {
        return value;
    }
}

contract A {
    uint a;

    function set(uint x) external {
        a = x;
    }
}

|]
      [r|contract SimpleStorage {
  @state
  int storedData;

  constructor() {
    this.storedData = 0;
  }
}

contract flipper {
  @state
  private bool value;

  function flip() : bool {
    this.value = !this.value;
    return true;
  }

  function get() : bool {
    return this.value;
  }
}

contract A {
  @state
  int a;

  public function set(int x, SigHashPreimage txPreimage) {
    this.a = x;
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    require(hash256(output) == SigHash.hashOutputs(txPreimage));
  }
}|]
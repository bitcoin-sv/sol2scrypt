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
        require(value, "an error message");
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
    require(this.value);
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


  describe "#ImportDirective" $ do
    itProgram "Program with one ImportDirective"
      [r|

import "./myLovelyLovelyLib.sol";

contract SimpleStorage {
    uint storedData;

    constructor() {}
}|]
      [r|import "./myLovelyLovelyLib.scrypt";

contract SimpleStorage {
  @state
  int storedData;

  constructor() {
  }
}|]


    itProgram "Program with multi ImportDirective"
      [r|

import "./myLovelyLovelyLib.sol";
import "ccc.sol";

contract SimpleStorage {
    uint storedData;

    constructor() {}
}|]
      [r|import "./myLovelyLovelyLib.scrypt";
import "ccc.scrypt";

contract SimpleStorage {
  @state
  int storedData;

  constructor() {
  }
}|]

    itProgram "Program with multi ImportDirective and pragma"
      [r|

// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

import "./myLovelyLovelyLib.sol";
import "ccc.sol";

contract SimpleStorage {
    uint storedData;

    constructor() {}
}|]
      [r|import "./myLovelyLovelyLib.scrypt";
import "ccc.scrypt";

contract SimpleStorage {
  @state
  int storedData;

  constructor() {
  }
}|]


  itProgram "Coin"
      [r|
pragma solidity ^0.8.10;

contract Coin {
    // The keyword "public" makes those variables
    // readable from outside.
    address public minter;
    mapping (address => uint) public balances;

    // Events allow light clients to react on
    // changes efficiently.
    event Sent(address from, address to, uint amount);

    // This is the constructor whose code is
    // run only when the contract is created.
    constructor() {
        minter = msg.sender;
    }

    function mint(address receiver, uint amount) external {
        if (msg.sender != minter) return;
        balances[receiver] += amount;
    }

    function send(address receiver, uint amount) external {
        if (balances[msg.sender] < amount) return;
        balances[msg.sender] -= amount;
        balances[receiver] += amount;
        emit Sent(msg.sender, receiver, amount);
    }
}|]
      [r|contract Coin {
  @state
  public PubKeyHash minter;

  @state
  public HashedMap<PubKeyHash, int> balances;

  constructor(PubKeyHash msgSender) {
    this.minter = msgSender;
  }

  public function mint(PubKeyHash receiver, int amount, SigHashPreimage txPreimage, Sig sig, PubKey pubKey, int this_balances_receiver, int this_balances_receiver_index) {
    PubKeyHash msgSender = hash160(pubKey);
    require(checkSig(sig, pubKey));
    require((!this.balances.has(receiver, this_balances_receiver_index)) || this.balances.canGet(receiver, this_balances_receiver, this_balances_receiver_index));
    if (msgSender != this.minter) {
      exit(false);
    }
    this_balances_receiver += amount;
    require(this.balances.set(receiver, this_balances_receiver, this_balances_receiver_index));
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    require(hash256(output) == SigHash.hashOutputs(txPreimage));
  }

  public function send(PubKeyHash receiver, int amount, SigHashPreimage txPreimage, Sig sig, PubKey pubKey, int this_balances_msgSender, int this_balances_msgSender_index, int this_balances_receiver, int this_balances_receiver_index) {
    PubKeyHash msgSender = hash160(pubKey);
    require(checkSig(sig, pubKey));
    require((!this.balances.has(msgSender, this_balances_msgSender_index)) || this.balances.canGet(msgSender, this_balances_msgSender, this_balances_msgSender_index));
    require((!this.balances.has(receiver, this_balances_receiver_index)) || this.balances.canGet(receiver, this_balances_receiver, this_balances_receiver_index));
    if (this_balances_msgSender < amount) {
      exit(false);
    }
    this_balances_msgSender -= amount;
    this_balances_receiver += amount;
    require(this.balances.set(msgSender, this_balances_msgSender, this_balances_msgSender_index));
    require(this.balances.set(receiver, this_balances_receiver, this_balances_receiver_index));
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    require(hash256(output) == SigHash.hashOutputs(txPreimage));
  }
}|]

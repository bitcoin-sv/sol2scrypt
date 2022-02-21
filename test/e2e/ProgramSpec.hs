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

  itProgram "contract with nested mapping"
    [r|contract EIP20 {
  mapping (address => mapping (address => uint256)) public allowed;

  function allowance(address _owner, address _spender) public view returns (uint256) {
    return allowed[_owner][_spender];
  }

  function approve(address _spender, uint256 _value) external returns (bool) {
    allowed[msg.sender][_spender] = _value;
    emit Approval(msg.sender, _spender, _value);
    return true;
  }
}|]
    [r|struct MapKeyST0 {
  PubKeyHash key0;
  PubKeyHash key1;
}

contract EIP20 {
  @state
  public HashedMap<MapKeyST0, int> allowed;

  function allowance(PubKeyHash _owner, PubKeyHash _spender, int this_allowed__owner__spender, int this_allowed__owner__spender_index) : int {
    require((!this.allowed.has({_owner, _spender}, this_allowed__owner__spender_index)) || this.allowed.canGet({_owner, _spender}, this_allowed__owner__spender, this_allowed__owner__spender_index));
    return this_allowed__owner__spender;
  }

  public function approve(PubKeyHash _spender, int _value, SigHashPreimage txPreimage, Sig sig, PubKey pubKey, int this_allowed_msgSender__spender, int this_allowed_msgSender__spender_index) {
    PubKeyHash msgSender = hash160(pubKey);
    require(checkSig(sig, pubKey));
    require((!this.allowed.has({msgSender, _spender}, this_allowed_msgSender__spender_index)) || this.allowed.canGet({msgSender, _spender}, this_allowed_msgSender__spender, this_allowed_msgSender__spender_index));
    this_allowed_msgSender__spender = _value;
    require(this.allowed.set({msgSender, _spender}, this_allowed_msgSender__spender, this_allowed_msgSender__spender_index));
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    require(hash256(output) == SigHash.hashOutputs(txPreimage));
  }
}|]

  itProgram "contract with multiple mappings"
    [r|contract MM{
  mapping (address => mapping (address => uint256)) public m1;
  mapping (address => mapping (address => uint256)) public m2;
  mapping (uint256 => mapping (address => uint256)) public m3;

  function f(address a) external {
    m1[a][a] = 1;
    m2[a][a]++;
    m3[1][a];
  }

}|]
    [r|struct MapKeyST0 {
  PubKeyHash key0;
  PubKeyHash key1;
}

struct MapKeyST1 {
  int key0;
  PubKeyHash key1;
}

contract MM {
  @state
  public HashedMap<MapKeyST0, int> m1;

  @state
  public HashedMap<MapKeyST0, int> m2;

  @state
  public HashedMap<MapKeyST1, int> m3;

  public function f(PubKeyHash a, SigHashPreimage txPreimage, int this_m1_a_a, int this_m1_a_a_index, int this_m2_a_a, int this_m2_a_a_index, int this_m3_1_a, int this_m3_1_a_index) {
    require((!this.m1.has({a, a}, this_m1_a_a_index)) || this.m1.canGet({a, a}, this_m1_a_a, this_m1_a_a_index));
    require((!this.m2.has({a, a}, this_m2_a_a_index)) || this.m2.canGet({a, a}, this_m2_a_a, this_m2_a_a_index));
    require((!this.m3.has({1, a}, this_m3_1_a_index)) || this.m3.canGet({1, a}, this_m3_1_a, this_m3_1_a_index));
    this_m1_a_a = 1;
    this_m2_a_a++;
    this_m3_1_a;
    require(this.m1.set({a, a}, this_m1_a_a, this_m1_a_a_index));
    require(this.m2.set({a, a}, this_m2_a_a, this_m2_a_a_index));
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    require(hash256(output) == SigHash.hashOutputs(txPreimage));
  }
}|]
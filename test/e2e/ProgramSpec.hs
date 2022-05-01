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
import Helper

-- transpile full solidity Program
transpileSol :: String -> IO (String, Logs)
transpileSol sol = do
  tr :: TranspileResult (Sol.SolidityCode SourceRange) IProgram' (Maybe (Scr.Program Ann)) <- transpile sol ""
  return (scryptCode tr, transpileLogs tr)

spec :: IO TestTree
spec = testSpec "Transpile Program" $ do
  let itProgram title sol scrypt = it ("should transpile Solidity `" ++ title ++ "` correctly") $ do
        tr  <- transpileSol sol
        tr `shouldBe` (scrypt, [])

  let itReportError title sol errs = it ("should report error when transpiling Solidity Program `" ++ title ++ "`") $ do
        (code, logs) <- transpileSol sol
        code `shouldBe` ""
        logs `shouldBe` map (uncurry (Log ErrorLevel)) errs

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

  public function flip(SigHashPreimage txPreimage) {
    require(this.value);
    this.value = !this.value;
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  public function get(bool retVal, SigHashPreimage txPreimage) {
    require(this.value == retVal);
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  function propagateState(SigHashPreimage txPreimage, int value) : bool {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, value);
    return hash256(output) == SigHash.hashOutputs(txPreimage);
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

    function flip() internal {
        value = !value;
    }

    function get() internal view returns (bool) {
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
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  function propagateState(SigHashPreimage txPreimage, int value) : bool {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, value);
    return hash256(output) == SigHash.hashOutputs(txPreimage);
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

    function flip() internal {
        value = !value;
    }

    function get() internal view returns (bool) {
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
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  function propagateState(SigHashPreimage txPreimage, int value) : bool {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, value);
    return hash256(output) == SigHash.hashOutputs(txPreimage);
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

  public function mint(PubKeyHash receiver, int amount, Sig sig, PubKey pubKey, int balances_receiver, int i0, SigHashPreimage txPreimage) {
    bool ret = false;
    bool returned = false;
    PubKeyHash msgSender = hash160(pubKey);
    require(checkSig(sig, pubKey));
    if (msgSender != this.minter) {
      {
        ret = true;
        returned = true;
      }
    }
    if (!returned) {
      require((!this.balances.has(receiver, i0) && balances_receiver == 0) || this.balances.canGet(receiver, balances_receiver, i0));
      balances_receiver += amount;
    }
    require(this.balances.set(receiver, balances_receiver, i0));
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  public function send(PubKeyHash receiver, int amount, Sig sig, PubKey pubKey, int balances_msgSender, int i0, int balances_receiver, int i1, SigHashPreimage txPreimage) {
    bool ret = false;
    bool returned = false;
    PubKeyHash msgSender = hash160(pubKey);
    require(checkSig(sig, pubKey));
    require((!this.balances.has(msgSender, i0) && balances_msgSender == 0) || this.balances.canGet(msgSender, balances_msgSender, i0));
    if (balances_msgSender < amount) {
      {
        ret = true;
        returned = true;
      }
    }
    if (!returned) {
      balances_msgSender -= amount;
      require((!this.balances.has(receiver, i1) && balances_receiver == 0) || this.balances.canGet(receiver, balances_receiver, i1));
      balances_receiver += amount;
    }
    require(this.balances.set(msgSender, balances_msgSender, i0));
    require(this.balances.set(receiver, balances_receiver, i1));
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  function propagateState(SigHashPreimage txPreimage, int value) : bool {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, value);
    return hash256(output) == SigHash.hashOutputs(txPreimage);
  }
}|]

  itProgram "contract with nested mapping"
    [r|contract EIP20 {
  mapping (address => mapping (address => uint256)) public allowed;

  function allowance(address _owner, address _spender) external view returns (uint256) {
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

  public function allowance(PubKeyHash _owner, PubKeyHash _spender, int retVal, int allowed__owner__spender, int i0, SigHashPreimage txPreimage) {
    require((!this.allowed.has({_owner, _spender}, i0) && allowed__owner__spender == 0) || this.allowed.canGet({_owner, _spender}, allowed__owner__spender, i0));
    require(allowed__owner__spender == retVal);
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  public function approve(PubKeyHash _spender, int _value, Sig sig, PubKey pubKey, bool retVal, int allowed_msgSender__spender, int i0, SigHashPreimage txPreimage) {
    PubKeyHash msgSender = hash160(pubKey);
    require(checkSig(sig, pubKey));
    require((!this.allowed.has({msgSender, _spender}, i0) && allowed_msgSender__spender == 0) || this.allowed.canGet({msgSender, _spender}, allowed_msgSender__spender, i0));
    allowed_msgSender__spender = _value;
    require(true == retVal);
    require(this.allowed.set({msgSender, _spender}, allowed_msgSender__spender, i0));
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  function propagateState(SigHashPreimage txPreimage, int value) : bool {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, value);
    return hash256(output) == SigHash.hashOutputs(txPreimage);
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

  public function f(PubKeyHash a, int m1_a_a, int i0, int m2_a_a, int i1, int m3_1_a, int i2, SigHashPreimage txPreimage) {
    require((!this.m1.has({a, a}, i0) && m1_a_a == 0) || this.m1.canGet({a, a}, m1_a_a, i0));
    m1_a_a = 1;
    require((!this.m2.has({a, a}, i1) && m2_a_a == 0) || this.m2.canGet({a, a}, m2_a_a, i1));
    m2_a_a++;
    require((!this.m3.has({1, a}, i2) && m3_1_a == 0) || this.m3.canGet({1, a}, m3_1_a, i2));
    m3_1_a;
    require(this.m1.set({a, a}, m1_a_a, i0));
    require(this.m2.set({a, a}, m2_a_a, i1));
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  function propagateState(SigHashPreimage txPreimage, int value) : bool {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, value);
    return hash256(output) == SigHash.hashOutputs(txPreimage);
  }
}|]


  itProgram "a simple ERC20 contract"
    [r|
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract ERC20  {
    uint public totalSupply;
    mapping(address => uint) public balanceOf;
    mapping(address => mapping(address => uint)) public allowance;
    string constant public name = "Solidity by Example";
    string constant public symbol = "SOLBYEX";
    uint8 constant public decimals = 18;

    event Transfer(address indexed from, address indexed to, uint value);
    event Approval(address indexed owner, address indexed spender, uint value);

    function transfer(address recipient, uint amount) external returns (bool) {
        balanceOf[msg.sender] -= amount;
        balanceOf[recipient] += amount;
        emit Transfer(msg.sender, recipient, amount);
        return true;
    }

    function approve(address spender, uint amount) external returns (bool) {
        allowance[msg.sender][spender] = amount;
        emit Approval(msg.sender, spender, amount);
        return true;
    }

    function transferFrom(
        address sender,
        address recipient,
        uint amount
    ) external returns (bool) {
        allowance[sender][msg.sender] -= amount;
        balanceOf[sender] -= amount;
        balanceOf[recipient] += amount;
        emit Transfer(sender, recipient, amount);
        return true;
    }

    function mint(uint amount) external {
        balanceOf[msg.sender] += amount;
        totalSupply += amount;
        emit Transfer(address(0), msg.sender, amount);
    }

    function burn(uint amount) external {
        balanceOf[msg.sender] -= amount;
        totalSupply -= amount;
        emit Transfer(msg.sender, address(0), amount);
    }
}|]
    [r|struct MapKeyST0 {
  PubKeyHash key0;
  PubKeyHash key1;
}

contract ERC20 {
  @state
  public int totalSupply;

  @state
  public HashedMap<PubKeyHash, int> balanceOf;

  @state
  public HashedMap<MapKeyST0, int> allowance;

  static const bytes name = "Solidity by Example";

  static const bytes symbol = "SOLBYEX";

  static const int decimals = 18;

  public function transfer(PubKeyHash recipient, int amount, Sig sig, PubKey pubKey, bool retVal, int balanceOf_msgSender, int i0, int balanceOf_recipient, int i1, SigHashPreimage txPreimage) {
    PubKeyHash msgSender = hash160(pubKey);
    require(checkSig(sig, pubKey));
    require((!this.balanceOf.has(msgSender, i0) && balanceOf_msgSender == 0) || this.balanceOf.canGet(msgSender, balanceOf_msgSender, i0));
    balanceOf_msgSender -= amount;
    require((!this.balanceOf.has(recipient, i1) && balanceOf_recipient == 0) || this.balanceOf.canGet(recipient, balanceOf_recipient, i1));
    balanceOf_recipient += amount;
    require(true == retVal);
    require(this.balanceOf.set(msgSender, balanceOf_msgSender, i0));
    require(this.balanceOf.set(recipient, balanceOf_recipient, i1));
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  public function approve(PubKeyHash spender, int amount, Sig sig, PubKey pubKey, bool retVal, int allowance_msgSender_spender, int i0, SigHashPreimage txPreimage) {
    PubKeyHash msgSender = hash160(pubKey);
    require(checkSig(sig, pubKey));
    require((!this.allowance.has({msgSender, spender}, i0) && allowance_msgSender_spender == 0) || this.allowance.canGet({msgSender, spender}, allowance_msgSender_spender, i0));
    allowance_msgSender_spender = amount;
    require(true == retVal);
    require(this.allowance.set({msgSender, spender}, allowance_msgSender_spender, i0));
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  public function transferFrom(PubKeyHash sender, PubKeyHash recipient, int amount, Sig sig, PubKey pubKey, bool retVal, int allowance_sender_msgSender, int i0, int balanceOf_recipient, int i2, int balanceOf_sender, int i1, SigHashPreimage txPreimage) {
    PubKeyHash msgSender = hash160(pubKey);
    require(checkSig(sig, pubKey));
    require((!this.allowance.has({sender, msgSender}, i0) && allowance_sender_msgSender == 0) || this.allowance.canGet({sender, msgSender}, allowance_sender_msgSender, i0));
    allowance_sender_msgSender -= amount;
    require((!this.balanceOf.has(sender, i1) && balanceOf_sender == 0) || this.balanceOf.canGet(sender, balanceOf_sender, i1));
    balanceOf_sender -= amount;
    require((!this.balanceOf.has(recipient, i2) && balanceOf_recipient == 0) || this.balanceOf.canGet(recipient, balanceOf_recipient, i2));
    balanceOf_recipient += amount;
    require(true == retVal);
    require(this.allowance.set({sender, msgSender}, allowance_sender_msgSender, i0));
    require(this.balanceOf.set(recipient, balanceOf_recipient, i2));
    require(this.balanceOf.set(sender, balanceOf_sender, i1));
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  public function mint(int amount, Sig sig, PubKey pubKey, int balanceOf_msgSender, int i0, SigHashPreimage txPreimage) {
    PubKeyHash msgSender = hash160(pubKey);
    require(checkSig(sig, pubKey));
    require((!this.balanceOf.has(msgSender, i0) && balanceOf_msgSender == 0) || this.balanceOf.canGet(msgSender, balanceOf_msgSender, i0));
    balanceOf_msgSender += amount;
    this.totalSupply += amount;
    require(this.balanceOf.set(msgSender, balanceOf_msgSender, i0));
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  public function burn(int amount, Sig sig, PubKey pubKey, int balanceOf_msgSender, int i0, SigHashPreimage txPreimage) {
    PubKeyHash msgSender = hash160(pubKey);
    require(checkSig(sig, pubKey));
    require((!this.balanceOf.has(msgSender, i0) && balanceOf_msgSender == 0) || this.balanceOf.canGet(msgSender, balanceOf_msgSender, i0));
    balanceOf_msgSender -= amount;
    this.totalSupply -= amount;
    require(this.balanceOf.set(msgSender, balanceOf_msgSender, i0));
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  function propagateState(SigHashPreimage txPreimage, int value) : bool {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, value);
    return hash256(output) == SigHash.hashOutputs(txPreimage);
  }
}|]


  itProgram "a contract with revert and error defined "
   [r|
// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.8.10;

error Unauthorized();

contract VendingMachine {
    address owner;

    function withdraw() external {
        if (msg.sender != owner)
            revert Unauthorized();
    }
}
|] [r|contract VendingMachine {
  @state
  PubKeyHash owner;

  public function withdraw(Sig sig, PubKey pubKey, SigHashPreimage txPreimage) {
    PubKeyHash msgSender = hash160(pubKey);
    require(checkSig(sig, pubKey));
    if (msgSender != this.owner)
      require(false);
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  function propagateState(SigHashPreimage txPreimage, int value) : bool {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, value);
    return hash256(output) == SigHash.hashOutputs(txPreimage);
  }
}|]

  itProgram "a contract to test return  "
   [r|
pragma solidity ^0.8.10;

contract SimpleStorage {
    uint storedData;

    function set(uint x) external {
        storedData++;
        uint a = 3;
        if(x > a) {
            return;
        }

        storedData = x;
    }

    function set1(uint x) internal {

        uint a = 3;
        if(x > a) {
            return;
        }
        a++;
    }

    function set2(uint x) external returns (uint) {
        storedData++;
        uint a = 3;
        if(x > a) {
            return storedData;
        }

        storedData = x;
        return storedData;
    }

    function set3(uint x) external view returns (uint y) { y = x; }

    function foo() private {
        if (true) return;
        int a = 1;
    }

    function foo1() private {
        return;
    }

    function foo2() external {
        return;
    }

    function get() external payable { return ; }

    function get1() external view returns (uint) { return storedData; }

    function get2() external pure returns (uint) { return 1 + 1; }

    function get3() external view returns (uint) { {true; {true;} } return storedData; }

}
|] [r|contract SimpleStorage {
  @state
  int storedData;

  public function set(int x, SigHashPreimage txPreimage) {
    bool ret = false;
    bool returned = false;
    this.storedData++;
    int a = 3;
    if (x > a) {
      {
        ret = true;
        returned = true;
      }
    }
    if (!returned) {
      this.storedData = x;
    }
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  function set1(int x) : bool {
    bool ret = false;
    bool returned = false;
    int a = 3;
    if (x > a) {
      {
        ret = true;
        returned = true;
      }
    }
    if (!returned) {
      a++;
    }
    return ret;
  }

  public function set2(int x, int retVal, SigHashPreimage txPreimage) {
    int ret = 0;
    bool returned = false;
    this.storedData++;
    int a = 3;
    if (x > a) {
      {
        ret = this.storedData;
        returned = true;
      }
    }
    if (!returned) {
      this.storedData = x;
    }
    require((returned ? ret : this.storedData) == retVal);
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  public function set3(int x, int _y, SigHashPreimage txPreimage) {
    int y = 0;
    y = x;
    require(y == _y);
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  private function foo() : bool {
    bool ret = false;
    bool returned = false;
    if (true) {
      {
        ret = true;
        returned = true;
      }
    }
    if (!returned) {
      int a = 1;
    }
    return ret;
  }

  private function foo1() : bool {
    return true;
  }

  public function foo2(SigHashPreimage txPreimage) {
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  public function get(SigHashPreimage txPreimage) {
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  public function get1(int retVal, SigHashPreimage txPreimage) {
    require(this.storedData == retVal);
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  public function get2(int retVal, SigHashPreimage txPreimage) {
    require(1 + 1 == retVal);
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  public function get3(int retVal, SigHashPreimage txPreimage) {
    {
      true;
      {
        true;
      }
    }
    require(this.storedData == retVal);
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  function propagateState(SigHashPreimage txPreimage, int value) : bool {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, value);
    return hash256(output) == SigHash.hashOutputs(txPreimage);
  }
}|]


  itProgram "test function with return statment and without return statment" [r|
pragma solidity ^0.8.10;

contract SimpleStorage {
    uint s;
    function foo() external {
  
    }

    function foo1() public {
      return;
    }

    function foo2() internal {
      
    }

    function foo3() private {
      return;
    }
}

|] [r|contract SimpleStorage {
  @state
  int s;

  public function foo(SigHashPreimage txPreimage) {
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  public function foo1(SigHashPreimage txPreimage) {
    require(this.propagateState(txPreimage, SigHash.value(txPreimage)));
  }

  function foo2() : bool {
    return true;
  }

  private function foo3() : bool {
    return true;
  }

  function propagateState(SigHashPreimage txPreimage, int value) : bool {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, value);
    return hash256(output) == SigHash.hashOutputs(txPreimage);
  }
}|]

  describe "#ReportError" $ do

    itReportError 
      "abstract / inheritance contract"
      [r|
  pragma solidity ^0.8.10;
  abstract contract Feline {
  }

  contract Cat is Feline {
      
  }
|] [( "unsupported abstract contract definition", newSR (3, 3) (4, 4)), 
       ( "unsupported contract inheritance", newSR (6, 3) (8, 4) ) ]

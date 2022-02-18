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
        require(x > 3, "a error message");
        a = x;
    }
}|]
    [r|contract A {
  @state
  int a;

  public function set(int x, SigHashPreimage txPreimage) {
    require(x > 3);
    this.a = x;
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    require(hash256(output) == SigHash.hashOutputs(txPreimage));
  }
}|]

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
    [r|contract SimpleStorage {
  @state
  int storedData;

  public function set(int x, SigHashPreimage txPreimage) {
    this.storedData = x;
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    require(hash256(output) == SigHash.hashOutputs(txPreimage));
  }

  function get() : int {
    return this.storedData;
  }
}|]

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
    [r|contract A {
  @state
  int a;

  function set(int x) : bool {
    this.a = x;
    {
      int a = 2;
      a = x;
      x = a;
    }
    this.a = 3;
    x = this.a;
    return true;
  }

  function add(int a) : bool {
    a = 1;
    int b = a;
    return true;
  }
}|]

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
    [r|contract flipper {
  @state
  private bool value;

  function flip() : bool {
    this.value = !this.value;
    return true;
  }

  function get() : bool {
    return this.value;
  }
}|]

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
    [r|contract flipper {
  @state
  private bool value;

  public function flip(SigHashPreimage txPreimage) {
    this.value = !this.value;
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    require(hash256(output) == SigHash.hashOutputs(txPreimage));
  }

  function get() : bool {
    return this.value;
  }
}|]

  itTransContract
    [r|contract Counter {
    uint public count;

    function get() public view returns (uint) {
        return count;
    }

    // Function to increment count by 1
    function inc() public {
        count += 1;
    }

    function set(uint _count) public {
        count = _count;
    }
}|]
    [r|contract Counter {
  @state
  public int count;

  function get() : int {
    return this.count;
  }

  function inc() : bool {
    this.count += 1;
    return true;
  }

  function set(int _count) : bool {
    this.count = _count;
    return true;
  }
}|]


  describe "#ContractPartEventDefinition" $ do
    let itEvent sol = it "should transpile Solidity event correctly" $ do
          tr :: TranspileResult Sol.ContractPart IR.IContractBodyElement' (Maybe (Scr.Param Ann)) <- transpile sol 
          scryptCode tr `shouldBe` ""

    itEvent "event Sent(address from, address to, uint amount);"
    itEvent "event EventName(address bidder, uint amount);"
    itEvent "event Deposit(address indexed _from, bytes32 indexed _id, uint _value);"
    itEvent "event Log(address indexed sender, string message);"
    itEvent "event AnotherLog();"


    itTransContract
      [r|contract Event {
    event Log(address indexed sender, string message);
    event AnotherLog();

    function test() public {
        emit Log(msg.sender, "Hello World!");
        emit Log(msg.sender, "Hello EVM!");
        emit AnotherLog();
    }
}|]
      [r|contract Event {
  function test() : bool {
    return true;
  }
}|]

  describe "#test contract constructor" $ do

    itTransContract
      [r|contract SimpleStorage {
    uint storedData;

    constructor(uint sd) {
        storedData = sd;
    }

    function set(uint x) external {
        storedData = x;
    }

    function get() public view returns (uint) {
        return storedData;
    }
}|]
      [r|contract SimpleStorage {
  @state
  int storedData;

  constructor(int sd) {
    this.storedData = sd;
  }

  public function set(int x, SigHashPreimage txPreimage) {
    this.storedData = x;
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    require(hash256(output) == SigHash.hashOutputs(txPreimage));
  }

  function get() : int {
    return this.storedData;
  }
}|]

  itTransContract
      [r|contract SimpleStorage {
    uint storedData;

    constructor() {
    }
}|]
      [r|contract SimpleStorage {
  @state
  int storedData;

  constructor() {
  }
}|]

  itTransContract
      [r|contract SimpleStorage {
    uint storedData;

    constructor(uint sd) {
    }
}|]
      [r|contract SimpleStorage {
  @state
  int storedData;

  constructor(int sd) {
  }
}|]

  itTransContract
      [r|contract SimpleStorage {
    uint storedData;

    constructor() {
        storedData = 0;
    }
}|]
      [r|contract SimpleStorage {
  @state
  int storedData;

  constructor() {
    this.storedData = 0;
  }
}|]

  itTransContract
      [r|contract SimpleStorage {
    uint storedData;

    constructor(uint sd, int i) {
        storedData = sd + i;
    }
}|]
      [r|contract SimpleStorage {
  @state
  int storedData;

  constructor(int sd, int i) {
    this.storedData = sd + i;
  }
}|]


  itTransContract
      [r|contract SimpleStorage {
    uint storedData;
    uint storedData1;
    uint constant x = 1;

    constructor() {}
}|]
      [r|contract SimpleStorage {
  @state
  int storedData;

  @state
  int storedData1;

  static const int x = 1;

  constructor() {
  }
}|]

  itTransContract
      [r|contract SimpleStorage {
    bool constant a = true;
    uint storedData;
    uint constant x = 1;

    constructor() {}
}|]
      [r|contract SimpleStorage {
  @state
  int storedData;

  static const bool a = true;

  static const int x = 1;

  constructor() {
  }
}|]


  itTransContract
      [r|contract Coin {
    address public minter;

    event Sent(address from, address to, uint amount);

    constructor() {
        minter = msg.sender;
    }

    function mint(address receiver, uint amount) external {
        if (msg.sender != minter) return;
    }

    function send(address receiver, uint amount) external {

        emit Sent(msg.sender, receiver, amount);
    }
}|]
      [r|contract Coin {
  @state
  public PubKeyHash minter;

  constructor(PubKeyHash msgSender) {
    this.minter = msgSender;
  }

  public function mint(PubKeyHash receiver, int amount, SigHashPreimage txPreimage, Sig sig, PubKey pubKey) {
    PubKeyHash msgSender = hash160(pubKey);
    require(checkSig(sig, pubKey));
    if (msgSender != this.minter) {
      exit(false);
    }
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    require(hash256(output) == SigHash.hashOutputs(txPreimage));
  }

  public function send(PubKeyHash receiver, int amount, SigHashPreimage txPreimage) {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    require(hash256(output) == SigHash.hashOutputs(txPreimage));
  }
}|]


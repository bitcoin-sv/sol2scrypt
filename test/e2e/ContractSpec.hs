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
import Helper

-- transpile full solidity contract
transpileSol :: String -> IO (String, Logs)
transpileSol sol = do
  tr :: TranspileResult (Sol.ContractDefinition SourceRange) IContract' (Maybe (Scr.Contract Ann)) <- transpile sol ""
  return (scryptCode tr, transpileLogs tr)

-- transpile part of solidity contract to Scr.Param 
transpileSolContractPart2Param :: String -> IO (String, Logs)
transpileSolContractPart2Param sol = do
  tr :: TranspileResult (Sol.ContractPart SourceRange) IContractBodyElement' (Maybe (Scr.Param Ann)) <- transpile sol ""
  return (scryptCode tr, transpileLogs tr)


spec :: IO TestTree
spec = testSpec "Transpile Contract" $ do
  let itTransContract title sol scrypt = it ("should transpile Solidity " ++ title ++ " correctly") $ do
        tr  <- transpileSol sol
        tr `shouldBe` (scrypt, [])

  let itReportError sol errs = it ("should report error when transpiling Solidity Contract `" ++ sol ++ "`") $ do
        (code, logs) <- transpileSol sol
        code `shouldBe` ""
        logs `shouldBe` map (uncurry (Log ErrorLevel)) errs

  let itReportErrorContractPart sol errs = it ("should report error when transpiling Solidity ContractPart `" ++ sol ++ "`") $ do
        (code, logs) <- transpileSolContractPart2Param sol
        code `shouldBe` ""
        logs `shouldBe` map (uncurry (Log ErrorLevel)) errs

  itTransContract "contract A with state `a` and no constructor"
    [r|contract A {
    uint a;

    function set(uint x) external {
        require(x > 3, "an error message");
        a = x;
    }
}|]
    [r|contract A {
  @state
  int a;

  public function set(int x, SigHashPreimage txPreimage) {
    require(x > 3);
    this.a = x;
    require(this.propagateState(txPreimage));
  }

  function propagateState(SigHashPreimage txPreimage) : bool {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    return hash256(output) == SigHash.hashOutputs(txPreimage);
  }
}|]

  itTransContract "contract SimpleStorage with state `storedData` and no constructor"
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
    require(this.propagateState(txPreimage));
  }

  function get() : int {
    return this.storedData;
  }

  function propagateState(SigHashPreimage txPreimage) : bool {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    return hash256(output) == SigHash.hashOutputs(txPreimage);
  }
}|]

  itTransContract "contract A with only public function"
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

  itTransContract "contract flipper with private state and no external function"
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

  itTransContract "contract flipper with external function"
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
    require(this.propagateState(txPreimage));
  }

  function get() : bool {
    return this.value;
  }

  function propagateState(SigHashPreimage txPreimage) : bool {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    return hash256(output) == SigHash.hashOutputs(txPreimage);
  }
}|]

  itTransContract "contract Counter without constructor"
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
          tr :: TranspileResult (Sol.ContractPart SourceRange) IR.IContractBodyElement' (Maybe (Scr.Param Ann)) <- transpile sol "" 
          scryptCode tr `shouldBe` ""

    itEvent "event Sent(address from, address to, uint amount);"
    itEvent "event EventName(address bidder, uint amount);"
    itEvent "event Deposit(address indexed _from, bytes32 indexed _id, uint _value);"
    itEvent "event Log(address indexed sender, string message);"
    itEvent "event AnotherLog();"


    itTransContract "contract Event with event"
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

    itTransContract "contract SimpleStorage with constructor"
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
    require(this.propagateState(txPreimage));
  }

  function get() : int {
    return this.storedData;
  }

  function propagateState(SigHashPreimage txPreimage) : bool {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    return hash256(output) == SigHash.hashOutputs(txPreimage);
  }
}|]

  itTransContract "contract SimpleStorage with a constructor without parameter"
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

  itTransContract "contract SimpleStorage with a constructor with a parameter"
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

  itTransContract  "contract SimpleStorage with a constructor with a statement"
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

  itTransContract "contract SimpleStorage with a constructor with multi parameter"
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


  itTransContract "contract SimpleStorage without public function"
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

  itTransContract "contract SimpleStorage with immutable and constant property"
      [r|contract SimpleStorage {
    bool constant a = true;
    uint storedData;
    uint constant x = 1;
    uint immutable decimals;
    uint immutable maxBalance;
    address immutable owner;

    constructor() {}
}|]
      [r|contract SimpleStorage {
  @state
  int storedData;

  const int decimals;

  const int maxBalance;

  const PubKeyHash owner;

  static const bool a = true;

  static const int x = 1;

  constructor() {
  }
}|]


  itTransContract "contract Coin with accessing Coin.msg.sender in constructor"
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
    require(this.propagateState(txPreimage));
  }

  public function send(PubKeyHash receiver, int amount, SigHashPreimage txPreimage) {
    require(this.propagateState(txPreimage));
  }

  function propagateState(SigHashPreimage txPreimage) : bool {
    require(Tx.checkPreimage(txPreimage));
    bytes outputScript = this.getStateScript();
    bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
    return hash256(output) == SigHash.hashOutputs(txPreimage);
  }
}|]

  describe "#ReportError" $ do
    itReportErrorContractPart "modifier onlyAfter(uint _time) { require(_time > 0); _; }" 
      [("unsupported contract part `ContractPartModifierDefinition`", 
        newSR (1, 1) (1, 58))]
    itReportErrorContractPart "enum State { Created, Locked, Inactive }" 
      [("unsupported contract part `ContractPartEnumDefinition`", 
        newSR (1, 2) (1, 41))]

    itReportErrorContractPart "using Set for Data;" 
      [("unsupported contract part `ContractPartUsingForDeclaration`", 
        newSR (1, 1) (1, 20))]
    itReportError "library D {}" [("unsupported contract definition `ContractDefinition`", 
      newSR (1, 1) (1, 13))]
    itReportError "abstract contract Feline { }" [("unsupported abstract contract definition", 
      newSR (1, 1) (1, 29))]
    itReportError "interface D {}" [("unsupported contract definition `ContractDefinition`", 
      newSR (1, 1) (1, 15))]
    itReportError "contract Cat is Feline { }" [("unsupported contract definition `ContractDefinition`", 
      newSR (1, 1) (1, 27))]


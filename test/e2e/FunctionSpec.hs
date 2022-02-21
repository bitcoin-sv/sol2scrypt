{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module FunctionSpec where

import qualified Data.Map.Lazy as Map
import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transpiler
import Text.RawString.QQ
import Utils

spec :: IO TestTree
spec = testSpec "Transpile Function" $ do
  let itTranspile title sol scr = it ("should transpile Solidity " ++ title ++ " correctly") $ do
        tr :: TranspileResult Sol.ContractPart IFunction' (Maybe (Scr.Function Ann)) <- transpile sol
        scryptCode tr `shouldBe` scr

  itTranspile
    "external pure function without return"
    "function set(uint x) external pure { uint y = x; }"
    [r|
public function set(int x) {
  int y = x;
  require(true);
}|]

  itTranspile
    "external pure function with named return"
    "function set(uint x) external pure returns (uint y) { y = x; }"
    [r|
public function set(int x, int _y) {
  int y = 0;
  y = x;
  require(y == _y);
}|]

  itTranspile
    "external pure function with unnamed return"
    "function set(uint x) external pure returns (uint) { y = x; return y; }"
    [r|
public function set(int x, int retVal) {
  y = x;
  require(y == retVal);
}|]

  itTranspile
    "external function without return"
    "function set(uint x) external { storedData = x; }"
    [r|
public function set(int x, SigHashPreimage txPreimage) {
  storedData = x;
  require(Tx.checkPreimage(txPreimage));
  bytes outputScript = this.getStateScript();
  bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
  require(hash256(output) == SigHash.hashOutputs(txPreimage));
}|]

  itTranspile
    "external view function with named return"
    "function set(uint x) external view returns (uint y) { y = x; }"
    [r|
public function set(int x, SigHashPreimage txPreimage, int _y) {
  int y = 0;
  y = x;
  require(Tx.checkPreimage(txPreimage));
  bytes outputScript = this.getStateScript();
  bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
  require(hash256(output) == SigHash.hashOutputs(txPreimage));
  require(y == _y);
}|]

  itTranspile
    "public view function without return"
    "function get() public view { storedData = 1; }"
    [r|
function get() : bool {
  storedData = 1;
  return true;
}|]
  
  itTranspile
    "public view function with named return"
    "function get() public view returns (uint y) { y = storedData; }"
    [r|
function get() : int {
  int y = 0;
  y = storedData;
  return y;
}|]

  itTranspile
    "public view function with unnamed return"
    "function get() public view returns (uint) { return storedData; }"
    [r|
function get() : int {
  return storedData;
}|]


  describe "#public " $ do

    itTranspile
      "public function"
      "function get() public { return x; }"
      "\nfunction get() : bool {\n  return x;\n}"

    itTranspile
      "pure function"
      "function get() public pure { return x; }"
      "\nfunction get() : bool {\n  return x;\n}"

    itTranspile
      "constant function"
      "function get() public constant { return x; }"
      "\nfunction get() : bool {\n  return x;\n}"

    itTranspile
      "view function"
      "function get() public view { return x; }"
      "\nfunction get() : bool {\n  return x;\n}"

    itTranspile
      "payable function"
      "function get() public payable { return x; }"
      "\nfunction get() : bool {\n  return x;\n}"

    itTranspile
      "public function with returns"
      "function privateFunc() public pure returns (bool memory) { return true; }"
      "\nfunction privateFunc() : bool {\n  return true;\n}"

    itTranspile
      "public view function with returns"
      "function get() public view returns (uint) { return storedData; }"
      "\nfunction get() : int {\n  return storedData;\n}"

    itTranspile
      "function empty"
      "function g() public { }"
      "\nfunction g() : bool {\n  return true;\n}"

    itTranspile
      "function with Expression"
      "function g(uint a) public pure returns (uint ret) { return a + f(); }"
      [r|
function g(int a) : int {
  int userDefined_ret = 0;
  return a + f();
}|]

    itTranspile
      "function with return in if branch"
      [r|function test0( uint amount) public view returns (uint) {
  uint x = 3;
  if(x == 0) {
      x++;
      return x;
  }
  x = x + amount;
  return x;
}|]
      [r|
function test0(int amount) : int {
  int ret = 0;
  bool returned = false;
  int x = 3;
  if (x == 0) {
    x++;
    {
      ret = x;
      returned = true;
    }
  }
  if (!returned) {
    x = x + amount;
  }
  return returned ? ret : x;
}|]

    itTranspile
      "function with return in else branch"
      [r|function test4( uint amount, uint y) public returns (uint) {
  uint x = 3;
  if(x > 0) {
    x++;
  } else {
    --x;
    return x;
  }
  x = x + amount;
  x += 20 / amount - 12;
  return x;
}|]
      [r|
function test4(int amount, int y) : int {
  int ret = 0;
  bool returned = false;
  int x = 3;
  if (x > 0) {
    x++;
  }
  else {
    --x;
    {
      ret = x;
      returned = true;
    }
  }
  if (!returned) {
    x = x + amount;
    x += 20 / amount - 12;
  }
  return returned ? ret : x;
}|]

    itTranspile
      "function with return in both if & else branch"
      [r|function test1( uint amount) public returns (uint) {
    uint x = 3;
    if(x == 3) {
        return x;
    } else {
        return x++;
    }
}|]
      [r|
function test1(int amount) : int {
  int ret = 0;
  bool returned = false;
  int x = 3;
  if (x == 3) {
    {
      ret = x;
      returned = true;
    }
  }
  else {
    {
      ret = x++;
      returned = true;
    }
  }
  return ret;
}|]

    itTranspile
      "function with return in nested if branch"
      [r|function test5( uint amount, uint y) public returns (uint) {
    uint x = 3;
    if(x > 0) {
        x++;
        if(x == 3) {
            return x;
        } else if(x == 5) {
            x++;
        } else if(x == 2) {
            x--;
        }
        x += 3;
        return x;
    } else {
        --x;
    }

    x = x + amount;
    x += 20 / amount - 12;
    return x;
}|]
      [r|
function test5(int amount, int y) : int {
  int ret = 0;
  bool returned = false;
  int x = 3;
  if (x > 0) {
    x++;
    if (x == 3) {
      {
        ret = x;
        returned = true;
      }
    }
    else if (x == 5) {
      x++;
    }
    else if (x == 2) {
      x--;
    }
    if (!returned) {
      x += 3;
      {
        ret = x;
        returned = true;
      }
    }
  }
  else {
    --x;
  }
  if (!returned) {
    x = x + amount;
    x += 20 / amount - 12;
  }
  return returned ? ret : x;
}|]

    itTranspile
      "function with return in non-block if branch"
      [r|function test6( uint x) public view returns (bool) {
    if(x == 0) 
        return true;
    return false;
}|]
      [r|
function test6(int x) : bool {
  bool ret = false;
  bool returned = false;
  if (x == 0) {
    {
      ret = true;
      returned = true;
    }
  }
  return returned ? ret : false;
}|]

    itTranspile
      "function with no return at the end"
      [r|function test9(uint x) public pure returns (uint) {
    uint y = 1;
}|]
      [r|
function test9(int x) : int {
  int y = 1;
  return 0;
}|]

    itTranspile
      "function with named but omitted return at the end"
      [r|function test10(uint x) public view returns (uint z) {
    if(x == 0) {
        if(x > 1){ 
            if(x > 9) {
                return ((x * x + y) > x*y) ? x-- : ((y*x) -9) ;
            } else {
                z = x*9- y + (x*y/100);
            }
            return (x-- - 200) * y  ;
        }
        return x -9;
    }
    z++;
}|]
      [r|
function test10(int x) : int {
  int ret = 0;
  bool returned = false;
  int z = 0;
  if (x == 0) {
    if (x > 1) {
      if (x > 9) {
        {
          ret = ((x * x + y) > x * y) ? x-- : ((y * x) - 9);
          returned = true;
        }
      }
      else {
        z = x * 9 - y + (x * y / 100);
      }
      if (!returned) {
        {
          ret = (x-- - 200) * y;
          returned = true;
        }
      }
    }
    if (!returned) {
      {
        ret = x - 9;
        returned = true;
      }
    }
  }
  if (!returned) {
    z++;
  }
  return returned ? ret : z;
}|]

    itTranspile
      "function with return bytes"
      [r|function test9(uint x) public pure returns (bytes) {
    bytes y = hex"00";
    if(x == 10000) {
      return y;
    }
    if(x >= 0) {

      if(x < -1) {
        if(x == -2) {
          y = hex"22";
          return y;
        } else {
          return y;
        }
      }

      return y;
    } else {
      y = hex"11";
      if(x < - 1) {
        if(x == -2) {
          y = hex"22";
          return y;
        } else {
          return y;
        }
      }
      y = hex"2233";
      x++;
    }

    x++;
    x=10;

    return y;
}|]
      [r|
function test9(int x) : bytes {
  bytes ret = b'';
  bool returned = false;
  bytes y = b'00';
  if (x == 10000) {
    {
      ret = y;
      returned = true;
    }
  }
  if (!returned) {
    if (x >= 0) {
      if (x < -1) {
        if (x == -2) {
          y = b'22';
          {
            ret = y;
            returned = true;
          }
        }
        else {
          {
            ret = y;
            returned = true;
          }
        }
      }
      if (!returned) {
        {
          ret = y;
          returned = true;
        }
      }
    }
    else {
      y = b'11';
      if (x < -1) {
        if (x == -2) {
          y = b'22';
          {
            ret = y;
            returned = true;
          }
        }
        else {
          {
            ret = y;
            returned = true;
          }
        }
      }
      if (!returned) {
        y = b'2233';
        x++;
      }
    }
    if (!returned) {
      x++;
      x = 10;
    }
  }
  return returned ? ret : y;
}|]

    itTranspile
      "function with return bool"
      [r|function test6( uint x) public view returns (bool) {
    if(x == 0) 
        return true;
    
    return false;
}|]
      [r|
function test6(int x) : bool {
  bool ret = false;
  bool returned = false;
  if (x == 0) {
    {
      ret = true;
      returned = true;
    }
  }
  return returned ? ret : false;
}|]

  describe "#private " $ do

    itTranspile
      "private function"
      "function get() private { return x; }"
      "\nprivate function get() : bool {\n  return x;\n}"

    itTranspile
      "pure function"
      "function get() private pure { return x; }"
      "\nprivate function get() : bool {\n  return x;\n}"

    itTranspile
      "constant function"
      "function get() private constant { return x; }"
      "\nprivate function get() : bool {\n  return x;\n}"

    itTranspile
      "view function"
      "function get() private view { return x; }"
      "\nprivate function get() : bool {\n  return x;\n}"

    itTranspile
      "payable function"
      "function get() private payable { return x; }"
      "\nprivate function get() : bool {\n  return x;\n}"

    itTranspile
      "private function with returns"
      "function privateFunc() private pure returns (bool memory) { return true; }"
      "\nprivate function privateFunc() : bool {\n  return true;\n}"

  describe "#internal " $ do

    itTranspile
      "internal function"
      "function get() internal { return x; }"
      "\nprivate function get() : bool {\n  return x;\n}"

    itTranspile
      "pure function"
      "function get() internal pure { return x; }"
      "\nprivate function get() : bool {\n  return x;\n}"

    itTranspile
      "constant function"
      "function get() internal constant { return x; }"
      "\nprivate function get() : bool {\n  return x;\n}"

    itTranspile
      "view function"
      "function get() internal view { return x; }"
      "\nprivate function get() : bool {\n  return x;\n}"

    itTranspile
      "payable function"
      "function get() internal payable { return x; }"
      "\nprivate function get() : bool {\n  return x;\n}"

    itTranspile
      "internal function with returns"
      "function privateFunc() internal pure returns (bool memory) { return true; }"
      "\nprivate function privateFunc() : bool {\n  return true;\n}"

  describe "#external " $ do

    itTranspile
      "get function"
      "function get() external payable { return x; }"
      [r|
public function get(SigHashPreimage txPreimage) {
  require(Tx.checkPreimage(txPreimage));
  bytes outputScript = this.getStateScript();
  bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
  require(hash256(output) == SigHash.hashOutputs(txPreimage));
  require(x == retVal);
}|]

    itTranspile
      "pure get function"
      "function get() external pure { return x; }"
      "\npublic function get() {\n  require(x == retVal);\n}"

    itTranspile
      "constant get function"
      "function get() external constant { return x; }"
      [r|
public function get(SigHashPreimage txPreimage) {
  require(Tx.checkPreimage(txPreimage));
  bytes outputScript = this.getStateScript();
  bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
  require(hash256(output) == SigHash.hashOutputs(txPreimage));
  require(x == retVal);
}|]

    itTranspile
      "view get function"
      "function get() external view { return x; }"
      [r|
public function get(SigHashPreimage txPreimage) {
  require(Tx.checkPreimage(txPreimage));
  bytes outputScript = this.getStateScript();
  bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
  require(hash256(output) == SigHash.hashOutputs(txPreimage));
  require(x == retVal);
}|]

    itTranspile
      "payable get function"
      "function get() external payable { return x; }"
      -- "public function get(SigHashPreimage txPreimage) { require(Tx.checkPreimage(txPreimage)); bytes outputScript = this.getStateScript(); bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage)); require(hash256(output) == SigHash.hashOutputs(txPreimage)); require(x == retVal); }"
      [r|
public function get(SigHashPreimage txPreimage) {
  require(Tx.checkPreimage(txPreimage));
  bytes outputScript = this.getStateScript();
  bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
  require(hash256(output) == SigHash.hashOutputs(txPreimage));
  require(x == retVal);
}|]

    itTranspile
      "set function"
      "function set(uint x) external { storedData = x; }"
      [r|
public function set(int x, SigHashPreimage txPreimage) {
  storedData = x;
  require(Tx.checkPreimage(txPreimage));
  bytes outputScript = this.getStateScript();
  bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
  require(hash256(output) == SigHash.hashOutputs(txPreimage));
}|]

    itTranspile
      "constant set function"
      "function set(uint x) external constant { storedData = x; }"
      [r|
public function set(int x, SigHashPreimage txPreimage) {
  storedData = x;
  require(Tx.checkPreimage(txPreimage));
  bytes outputScript = this.getStateScript();
  bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
  require(hash256(output) == SigHash.hashOutputs(txPreimage));
}|]

    itTranspile
      "pure set function"
      "function set(uint x) external pure { storedData = x; }"
      [r|
public function set(int x) {
  storedData = x;
  require(true);
}|]

    itTranspile
      "view set function"
      "function set(uint x) external view { storedData = x; }"
      [r|
public function set(int x, SigHashPreimage txPreimage) {
  storedData = x;
  require(Tx.checkPreimage(txPreimage));
  bytes outputScript = this.getStateScript();
  bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
  require(hash256(output) == SigHash.hashOutputs(txPreimage));
}|]

    itTranspile
      "payable set function"
      "function set(uint x) external payable { storedData = x; }"
      [r|
public function set(int x, SigHashPreimage txPreimage) {
  storedData = x;
  require(Tx.checkPreimage(txPreimage));
  bytes outputScript = this.getStateScript();
  bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
  require(hash256(output) == SigHash.hashOutputs(txPreimage));
}|]

    itTranspile
      "external function with returns"
      "function get() external view returns (uint) { return storedData; }"
      [r|
public function get(SigHashPreimage txPreimage, int retVal) {
  require(Tx.checkPreimage(txPreimage));
  bytes outputScript = this.getStateScript();
  bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
  require(hash256(output) == SigHash.hashOutputs(txPreimage));
  require(storedData == retVal);
}|]


  describe "#indent" $ do
    describe "#should indent when printing block statement." $ do
      itTranspile
        "external function with returns"
        "function get() external view returns (uint) { {true; {true;} } return storedData; }"
        [r|
public function get(SigHashPreimage txPreimage, int retVal) {
  {
    true;
    {
      true;
    }
  }
  require(Tx.checkPreimage(txPreimage));
  bytes outputScript = this.getStateScript();
  bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
  require(hash256(output) == SigHash.hashOutputs(txPreimage));
  require(storedData == retVal);
}|]

  describe "#msg" $ do
    itTranspile
      "msg.sender"
      [r|function get(address addr) external view {
  if (msg.sender == addr)
    addr1 = msg.sender;
  else {
    address addr1 = msg.sender;
  }
  msg.sender;
}|]
      [r|
public function get(PubKeyHash addr, SigHashPreimage txPreimage, Sig sig, PubKey pubKey) {
  PubKeyHash msgSender = hash160(pubKey);
  require(checkSig(sig, pubKey));
  if (msgSender == addr)
    addr1 = msgSender;
  else {
    PubKeyHash addr1 = msgSender;
  }
  msgSender;
  require(Tx.checkPreimage(txPreimage));
  bytes outputScript = this.getStateScript();
  bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
  require(hash256(output) == SigHash.hashOutputs(txPreimage));
}|]

    itTranspile
      "msg.value"
      "function get() external view {uint amt = msg.value;}"
      [r|
public function get(SigHashPreimage txPreimage) {
  int msgValue = SigHash.value(txPreimage);
  int amt = msgValue;
  require(Tx.checkPreimage(txPreimage));
  bytes outputScript = this.getStateScript();
  bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
  require(hash256(output) == SigHash.hashOutputs(txPreimage));
}|]

  describe "#mapping" $ do
    let itTranspileWithMapping mapName sol scrypt = 
          it "should transpile function with mapping-typed var correctly" $ do
            let mapSym = Symbol (IR.Identifier mapName) (Mapping (ElementaryType Address) (ElementaryType IR.Int)) False
                initEnv =  [Map.insert (IR.Identifier mapName) mapSym Map.empty]
            tr :: TranspileResult Sol.ContractPart IFunction' (Maybe (Scr.Function Ann)) <- 
                          transpile' (TransformState initEnv Nothing Map.empty [] Map.empty) sol
            scryptCode tr `shouldBe` scrypt

    itTranspileWithMapping
      "balances"
      [r|function send(address receiver, uint amount) external {
    balances[msg.sender] -= amount;
    balances[receiver] += amount;
}|]
      [r|
public function send(PubKeyHash receiver, int amount, SigHashPreimage txPreimage, Sig sig, PubKey pubKey, int balances_msgSender, int balances_msgSender_index, int balances_receiver, int balances_receiver_index) {
  PubKeyHash msgSender = hash160(pubKey);
  require(checkSig(sig, pubKey));
  require((!balances.has(msgSender, balances_msgSender_index)) || balances.canGet(msgSender, balances_msgSender, balances_msgSender_index));
  require((!balances.has(receiver, balances_receiver_index)) || balances.canGet(receiver, balances_receiver, balances_receiver_index));
  balances_msgSender -= amount;
  balances_receiver += amount;
  require(balances.set(msgSender, balances_msgSender, balances_msgSender_index));
  require(balances.set(receiver, balances_receiver, balances_receiver_index));
  require(Tx.checkPreimage(txPreimage));
  bytes outputScript = this.getStateScript();
  bytes output = Utils.buildOutput(outputScript, SigHash.value(txPreimage));
  require(hash256(output) == SigHash.hashOutputs(txPreimage));
}|]


      




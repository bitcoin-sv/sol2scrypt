

# All supported features 



Feature | Solidity | supported | sCrypt | note
--- | --- | --- | --- | ---
Pragma Directive | `pragma solidity ^0.8.10;` <br> `pragma experimental ABIEncoderV2;` | **Yes** | *Empty* |
Import Directive | `import "./Foo.sol";` | **Yes** | `import "./Foo.sol";` | only support local file
Elementary Type **address** | `address` | **Yes** | `PubKeyHash` | 
Elementary Type **int** | `int` , `int8` , `int16` , ... , `int248` , `int256`  | **Yes** | `int` | 
Elementary Type **uint** | `uint` , `uint8` , `uint16` , ... , `uint248` , `uint256` | **Yes** | `int` | 
Elementary Type **bytes** | `bytes` , `bytes1` , `bytes2` , ...,  `bytes31` , `bytes32` | **Yes** | `bytes` |
Elementary Type **Fixed** | `fixed4x4` | <font color="red" >NO</font> |  |
Elementary Type **Ufixed** | `ufixed4x4` | <font color="red" >NO</font> |  |
Elementary Type **Array** | `uint[]` | <font color="red" >NO</font> |  |
Elementary Type **bool** | `bool` | **Yes** | `bool` |
Elementary Type **string** | `string` | **Yes** | `bytes` |
Comment | `// comment` , `/*  comment */` |  **Yes** | *Empty* |
Boolean Literal | `true` , `false` |  **Yes** | `true` , `false` |
Number Literal | `1` , `0x1` |  **Yes** | `1` , `0x1` |
NumberUnit | `1 days` , `1 wei` |  <font color="red" >NO</font> | |
Hex Literal | `hex"0101"` |  **Yes** | `b'0101'`  |
String Literal | `"hello world"` |  **Yes** | `"hello world"`  |
Array literal | `[1, 2, 3]` | **Yes** | `[1, 2, 3]`
Array length | `s.length;` |  <font color="red" >NO</font> |  
Array Index | `a[1]`, `a[i]` | **Yes** |  `a[1]`, `a[i]` | only support for read array value
Add/remove  Array element| `a.push(3);`, `a.pop();` | <font color="red" >NO</font> |   | 
Increment and Decrement | `i++`, `++i`, `i--`, `--i` |  **Yes** |  `i++`, `++i`, `i--`, `--i`
Binary Operator | `+=`, `-=`, `*=`, `/=`, `%=`   |  **Yes** | `+=`, `-=`, `*=`, `/=`, `%=` |
Binary Operator | `+`, `-`, `*`, `/`, `%`   |  **Yes** | `+`, `-`, `*`, `/`, `%` |
Binary Operator | `&=`, `\|=`, `^=`, `<<=`, `>>=`   |  **Yes** | `&=`, `\|=`, `^=`, `<<=`, `>>=` | only works on bytes in scrypt
Binary Operator | `==`, `!=`, `<`, `<=`, `>` , `>=`, `&&`, `\|\|` |  **Yes** | `==`, `!=`, `<`, `<=`, `>` , `>=`, `&&`, `\|\|` | 
Ternary Operator | `x > 0 ? x : -x` |  **Yes** | `x > 0 ? x : -x` | 
Assignment | `a = 1;` |  **Yes** | `a = 1;` |
Parallel Assignment  | `(x, y) = (0, 1);` |  <font color="red" >NO</font> |  |
Constant | `uint constant TOTAL_SUPPLY = 10000000;` |  **Yes** | `static const int TOTAL_SUPPLY = 10000000;` |
Immutable variables |`uint immutable x;`| **Yes**  | `const int x;` |
Contract Define | `contract Coin { ... };` |  **Yes**  | `contract Coin { ... };` |
Contract Property | <code>contract Coin { </br> &nbsp; address public minter; </br>&nbsp; ... </br> }<code> |  **Yes**  | <code>contract Coin {  </br> &nbsp;@state public PubKeyHash minter;</br>  &nbsp; ...  </br>  } </code> | *cannot support property with init value: `uint amount = 1000;`*
Contract Creation | `Contract c = new Contract(args);` |  <font color="red" >NO</font>  | |
Interfaces |`interface HelloWorld { ... }`| <font color="red" >NO</font>  | |
Library |`library HelloWorld { ... }`| <font color="red" >NO</font>  | |
Public function |<code> function get() public view returns (uint) { </br> &nbsp; return storedData; </br> }</code>| **Yes**  | <code> function get() : int { </br> &nbsp; return this.storedData; </br> } </code> |
External function |<code> function set(uint x) external {</br> &nbsp; storedData = x; </br>} </code>| **Yes**  | <code> public function set(int x, SigHashPreimage txPreimage) { </br>  &nbsp;  this.storedData = x; </br> &nbsp; require(this.propagateState(txPreimage)); </br>  } </code>| *auto add a function called `propagateState`*
Event |`event Event();`| **Yes**  | *Empty* |
Emit Event |`emit Log(msg.sender, "Hello EVM!");`| **Yes**  | *Empty* |
Mapping | `mapping (address => uint)` | **Yes** | `HashedMap<PubKeyHash, int>` | 
Nested Mapping | `mapping (address => mapping (address => uint))` | **Yes** | <code> struct MapKeyST0 { </br>  &nbsp; PubKeyHash key0; </br> &nbsp; PubKeyHash key1; </br>} </br>... </br>HashedMap<MapKeyST0, int></code> | 
If statement | <code>if (a > 2) { </br> &nbsp; ... </br> else if (a == 0) { </br> &nbsp;  ... </br> } else { </br> &nbsp; ... </br> } </code>| **Yes**  | <code>if (a > 2) { </br> &nbsp; ... </br> else if (a == 0) { </br> &nbsp;  ... </br> } else { </br> &nbsp; ... </br> } </code> |
For loop | <code>for (uint i = 0; i < 3; i++) { </br>  &nbsp;  ... </br> } </code>| <font color="red" >NO</font>   |  |
While loop | <code>while (a > 0)  { </br>  &nbsp;  ... </br> } </code>| <font color="red" >NO</font>  |  |
Do-While loop | <code>do { </br> &nbsp; ... </br> } while (a > 0); </code>| <font color="red" >NO</font>  |  |
Assembly | <code>assembly { </br> &nbsp; ... </br> } </code>| <font color="red" >NO</font>  |  |
Return in function without return type | `return ;` | **Yes**  | `exit(false) ;` |
Return in function with return type | `return a;` | **Yes**  | `return a;` | *at last statement*
Return in function with return type  | <code> function get(uint amount) public view returns (uint) { </br>      &nbsp; if (amount > 0) </br> &nbsp; &nbsp; return amount; </br> &nbsp; return 0; </br>  } </code>| **Yes**  | <code>  function get(int amount) : int { </br>  &nbsp; int ret = 0; </br> &nbsp; bool returned = false; </br> &nbsp; if (amount > 0) { </br> &nbsp; &nbsp;{ </br> &nbsp; &nbsp; &nbsp;ret = amount; </br> &nbsp;&nbsp;&nbsp;&nbsp; returned = true; </br> &nbsp;&nbsp; } </br> &nbsp;  } </br> &nbsp; return returned ? ret : 0; </br>  } </code>|  *at middle statement*
Break |`break;`| <font color="red" >NO</font>  | |
Continue |`continue;`| <font color="red" >NO</font>  | |
Assert | `assert(x > y);` | **Yes**  | `require(x > y);` |
Require | `require(x > y, "message");` | **Yes**  | `require(x > y);` |
Error Definition | `error Unauthorized();` | **Yes**  |  *Empty* |
Revert | `revert Unauthorized();` | **Yes**  | `require(false);` |
Try Catch | `try ... {} catch Error(...) { ... }` | <font color="red" >NO</font> |  |
Units, global constants and type ranges | <code> 1 ether <br> 1 wei <br>1 gwei </br>1 seconds</br>1 minutes</br>1 hours</br>1 days</br>1 weeks</br>1 years  // deprecated</br>type(uint).min</br>type(uint).max</br>type(int8).min</br>type(int8).max</br>...</br></code> |  <font color="red" >NO</font> |  |
Block and transaction properties| <code> blockhash(blockNumber) </br>block.coinbase </br>block.difficulty </br>block.gaslimit </br>block.number</br>block.timestamp</br>gasleft() </br>msg.data </br>msg.gas </br>msg.sig </br>tx.gasprice </br>tx.origin </br></code>|<font color="red" >NO</font> |  |
msg.sender | `msg.sender;` | **Yes**  |  <code>PubKeyHash msgSender = hash160(pubKey); </br> require(checkSig(sig, pubKey)); </code> | *will automatically add two parameters to the function signature： `Sig sig, PubKey pubKey`*
msg.sender | `msg.value;` | **Yes**  |  `SigHash.value(txPreimage);` | *will automatically add one parameters to the function signature： `SigHashPreimage txPreimage`*













# All supported features 


| Feature | Solidity | supported | sCrypt | note |
| :--- | :--- | :--- | :--- | :--- |
Pragma Directive | `pragma solidity ^0.8.10;` <br> `pragma experimental ABIEncoderV2;` | **Yes** | *Empty* |
Import Directive | `import "./Foo.sol";` | **Yes** | `import "./Foo.sol";` | only support local file
Elementary Type **address** | `address` | **Yes** | `PubKeyHash` | 
Elementary Type **int** | `int` , `int8` , `int16` , ... , `int248` , `int256`  | **Yes** | `int` | 
Elementary Type **uint** | `uint` , `uint8` , `uint16` , ... , `uint248` , `uint256` | **Yes** | `int` | 
Elementary Type **bytes** | `bytes` , `bytes1` , `bytes2` , ...,  `bytes31` , `bytes32` | **Yes** | `bytes` |
Elementary Type **Fixed** | `fixed4x4` | **NO** |  |
Elementary Type **Ufixed** | `ufixed4x4` | **NO** |  |
Elementary Type **Array** | `uint[]` | **NO** |  |
Elementary Type **bool** | `bool` | **Yes** | `bool` |
Elementary Type **string** | `string` | **Yes** | `bytes` |
Comment | `// comment` , `/*  comment */` |  **Yes** | *Empty* |
Boolean Literal | `true` , `false` |  **Yes** | `true` , `false` |
Number Literal | `1` , `0x1` |  **Yes** | `1` , `0x1` |
NumberUnit | `1 days` , `1 wei` |  **NO** | |
Hex Literal | `hex"0101"` |  **Yes** | `b'0101'`  |
String Literal | `"hello world"` |  **Yes** | `"hello world"`  |
Array literal | `[1, 2, 3]` | **Yes** | `[1, 2, 3]`
Array length | `s.length;` |  **NO** |  
Array Index | `a[1]`, `a[i]` | **Yes** |  `a[1]`, `a[i]` | only support for read array value
Add/remove  Array element| `a.push(3);`, `a.pop();` | **NO** |   | 
Increment and Decrement | `i++`, `++i`, `i--`, `--i` |  **Yes** |  `i++`, `++i`, `i--`, `--i`
Binary Operator | `+=`, `-=`, `*=`, `/=`, `%=`   |  **Yes** | `+=`, `-=`, `*=`, `/=`, `%=` |
Binary Operator | `+`, `-`, `*`, `/`, `%`   |  **Yes** | `+`, `-`, `*`, `/`, `%` |
Binary Operator | `&=`, `\|=`, `^=`, `<<=`, `>>=`   |  **Yes** | `&=`, `\|=`, `^=`, `<<=`, `>>=` | only works on bytes in scrypt
Binary Operator | `==`, `!=`, `<`, `<=`, `>` , `>=`, `&&`, `\|\|` |  **Yes** | `==`, `!=`, `<`, `<=`, `>` , `>=`, `&&`, `\|\|` | 
Ternary Operator | `x > 0 ? x : -x` |  **Yes** | `x > 0 ? x : -x` | 
Assignment | `a = 1;` |  **Yes** | `a = 1;` |
Parallel Assignment  | `(x, y) = (0, 1);` |  **NO** |  |
Constant | `uint constant TOTAL_SUPPLY = 10000000;` |  **Yes** | `static const int TOTAL_SUPPLY = 10000000;` |
Immutable variables |`uint immutable x;`| **Yes**  | `const int x;` |
Contract Define | `contract Coin { ... };` |  **Yes**  | `contract Coin { ... };` |
Contract Property | `contract Coin {` <br> &nbsp; `address public minter;` <br>&nbsp; `...` <br> `}` |  **Yes**  | `contract Coin {`  <br> &nbsp;`@state public PubKeyHash minter;`<br>  &nbsp; `...`  <br> `}` | *cannot support property with init value: `uint amount = 1000;`*
Contract Creation | `Contract c = new Contract(args);` |  **NO**  | |
Interfaces |`interface HelloWorld { ... }`| **NO**  | |
Library |`library HelloWorld { ... }`| **NO**  | |
Inheritance |`contract ERC20 is IERC20 { ... }`| **NO**  | |
Public function |`function get() public view returns (uint) {` <br> &nbsp; `return storedData;` <br> `}` | **Yes**  | `function get() : int {` <br> &nbsp; `return this.storedData;` <br> `}` |
External function | `function set(uint x) external {`<br> &nbsp; `storedData = x;` <br>`}`| **Yes**  | `public function set(int x, SigHashPreimage txPreimage) {` <br>  &nbsp;  `this.storedData = x;` <br> &nbsp; `require(this.propagateState(txPreimage));` <br>  `}`| *auto add a function called `propagateState`*
Event |`event Event();`| **Yes**  | *Empty* |
Emit Event |`emit Log(msg.sender, "Hello EVM!");`| **Yes**  | *Empty* |
Mapping | `mapping (address => uint)` | **Yes** | `HashedMap<PubKeyHash, int>` | 
Nested Mapping | `mapping (address => mapping (address => uint))` | **Yes** | `struct MapKeyST0 {` <br>  &nbsp;` PubKeyHash key0;` <br> &nbsp; `PubKeyHash key1;` <br>`}` <br>`...` <br>`HashedMap<MapKeyST0, int>` | 
If statement | `if (a > 2) {` <br> &nbsp; `...` <br> `else if (a == 0) {` <br> &nbsp;  `...` <br> `} else {` <br> &nbsp; `...` <br> `}`| **Yes**  | `if (a > 2) {` <br> &nbsp; `...` <br> `else if (a == 0) {` <br> &nbsp;  `...` <br> `} else {` <br> &nbsp; `...` <br> `}` |
For loop | `for (uint i = 0; i < 3; i++) {` <br>  &nbsp;  `...` <br> `}` | **NO**   |  |
While loop | `while (a > 0) {` <br>  &nbsp;  `...` <br> `}`| **NO**  |  |
Do-While loop | `do {` <br> &nbsp; `...` <br> `} while (a > 0);`| **NO**  |  |
Assembly | `assembly {` <br> &nbsp; `...` <br> `}`| **NO**  |  |
Return in function without return type | `return ;` | **Yes**  | `exit(false) ;` |
Return in function with return type | `return a;` | **Yes**  | `return a;` | *at last statement*
Return in function with return type  | `function get(uint amount) public view returns (uint) {` <br>      &nbsp; `if (amount > 0)` <br> &nbsp; &nbsp; `return amount;` <br> &nbsp; `return 0;` <br>  `}`| **Yes**  | `function get(int amount) : int {` <br>  &nbsp; `int ret = 0;` <br> &nbsp; `bool returned = false;` <br> &nbsp; `if (amount > 0) {` <br> &nbsp; &nbsp;`{` <br> &nbsp; &nbsp; &nbsp;`ret = amount;` <br> &nbsp;&nbsp;&nbsp;&nbsp; `returned = true;` <br> &nbsp;&nbsp; `}` <br> &nbsp;  `}` <br> &nbsp; `return returned ? ret : 0;` <br>  `}`|  *at middle statement*
Break |`break;`| **NO**  | |
Continue |`continue;`| **NO**  | |
Assert | `assert(x > y);` | **Yes**  | `require(x > y);` |
Require | `require(x > y, "message");` | **Yes**  | `require(x > y);` |
Error Definition | `error Unauthorized();` | **Yes**  |  *Empty* |
Revert | `revert Unauthorized();` | **Yes**  | `require(false);` |
Try Catch | `try ... {}` <br> `catch Error(...)` <br> `{ ... }` | **NO** |  |
Units, global constants and type ranges | `1 ether`<br>`1 wei`<br>`1 gwei` <br>`1 seconds`<br>`1 minutes`<br>`1 hours`<br>`1 days`<br>`1 weeks`<br>`type(uint).min`<br>`type(uint).max`<br>`type(int8).min`<br>`type(uint8).max`<br>...<br> |  **NO** |  |
Block and transaction properties| `blockhash(blockNumber)` <br>`block.coinbase` <br>`block.difficulty` <br>`block.gaslimit` <br>`block.number`<br>`block.timestamp`<br>`gasleft()` <br>`msg.data` <br>`msg.gas` <br>`msg.sig` <br>`tx.gasprice` <br>`tx.origin` <br>|**NO** |  |
msg.sender | `msg.sender;` | **Yes**  |  `PubKeyHash msgSender = hash160(pubKey);` <br> `require(checkSig(sig, pubKey));` | *will automatically add two parameters to the function signature： `Sig sig, PubKey pubKey`*
msg.value | `msg.value;` | **Yes**  |  `SigHash.value(txPreimage);` | *will automatically add one parameters to the function signature： `SigHashPreimage txPreimage`*











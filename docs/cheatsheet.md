

# All supported features 

| Feature | Solidity | sCrypt | Note |
| :--- | :--- | :--- | ---: |
Pragma Directive | `pragma solidity ^0.8.10;` <br> `pragma experimental ABIEncoderV2;` | *empty* | |
Import Directive | `import "./Foo.sol";` | `import "./Foo.sol";` | only support local file
Type **address** | `address` | `PubKeyHash` | 
Type **int** | `int` , `int8` , `int16` , ... , `int248` , `int256`  | `int` | 
Type **uint** | `uint` , `uint8` , `uint16` , ... , `uint248` , `uint256` | `int` | 
Type **bytes** | `bytes` , `bytes1` , `bytes2` , ...,  `bytes31` , `bytes32` | `bytes` |
Type **Fixed** | `fixed4x4` | *Not Supported* |  |
Type **Ufixed** | `ufixed4x4` | *Not Supported* |  |
Type **Array** | `uint[]` <br> `uint[3]` | *Not Supported*  <br> `int[3]` |  |
Type **bool** | `bool` | `bool` |
Type **string** | `string` | `bytes` |
Comment | `// comment` , `/*  comment */` | *empty* |
Boolean Literal | `true` , `false` | `true` , `false` |
Number Literal | `1` <br> `0x1` | `1` <br> `0x1` |
NumberUnit | `1 days` <br> `1 wei` |  *Not Supported* | |
Hex Literal | `hex"0101"` |  `b'0101'`  |
String Literal | `"hello world"` | `"hello world"`  |
Array literal | `[1, 2, 3]` | `[1, 2, 3]`
Array length | `s.length;` |  *Not Supported* |  
Array Index | `a[1]` <br> `a[i]` | `a[1]` <br>  `a[i]`  *only support for read array value* |
Add/remove  Array element| `a.push(3);`, `a.pop();` | *Not Supported* |   | 
Increment and Decrement | `i++`, `++i`, `i--`, `--i` | `i++`, `++i`, `i--`, `--i`
Binary Operator | `+=`, `-=`, `*=`, `/=`, `%=` <br>  `+`, `-`, `*`, `/`, `%` <br> `&=`, `\|=`, `^=`, `<<=`, `>>=` <br> `==`, `!=`, `<`, `<=`, `>` , `>=`, `&&`, `\|\|` | `+=`, `-=`, `*=`, `/=`, `%=` <br> `+`, `-`, `*`, `/`, `%` <br> `&=`, `\|=`, `^=`, `<<=`, `>>=` <br> `==`, `!=`, `<`, `<=`, `>` , `>=`, `&&`, `\|\|` | `&=`, `\|=`, `^=`, `<<=`, `>>=` *only works on bytes in sCrypt*
Ternary Operator | `x > 0 ? x : -x` |  `x > 0 ? x : -x` | 
Assignment | `a = 1;` | `a = 1;` |
Parallel Assignment  | `(x, y) = (0, 1);` |  *Not Supported* |  |
Constant | `uint constant TOTAL_SUPPLY = 10000000;` | `static const int TOTAL_SUPPLY = 10000000;` |
Immutable variables |`uint immutable x;`| `const int x;` |
Contract Define | `contract Coin { ... };` |  `contract Coin { ... };` |
Contract Property | `contract Coin {` <br> &nbsp; `address public minter;` <br>&nbsp; `...` <br> `}` | `contract Coin {`  <br> &nbsp;`@state public PubKeyHash minter;`<br>  &nbsp; `...`  <br> `}` | *cannot support property with initialization: `uint amount = 1000;`*
Contract Creation | `Contract c = new Contract(args);` |  *Not Supported*  | |
Interfaces |`interface HelloWorld { ... }`| *Not Supported*  | |
Library |`library HelloWorld { ... }`| *Not Supported*  | |
Inheritance |`contract ERC20 is IERC20 { ... }`| *Not Supported*  | |
Public function |`function get() public view returns (uint) {` <br> &nbsp; `return storedData;` <br> `}` | `function get() : int {` <br> &nbsp; `return this.storedData;` <br> `}` |
External function | `function set(uint x) external {`<br> &nbsp; `storedData = x;` <br>`}`| `public function set(int x, SigHashPreimage txPreimage) {` <br>  &nbsp;  `this.storedData = x;` <br> &nbsp; `require(this.propagateState(txPreimage));` <br>  `}`| *automatically add require statement `require(this.propagateState(txPreimage));` and function paremeter `SigHashPreimage txPreimage` when transpiling external function*
Event |`event Event();` | *empty* |
Emit Event |`emit Log(msg.sender, "Hello EVM!");`| *empty* |
Mapping | `mapping (address => uint)` |  `HashedMap<PubKeyHash, int>` | [HashedMap](https://scryptdoc.readthedocs.io/en/latest/contracts.html#library-hashedmap)
Nested Mapping | `mapping (address => mapping (address => uint)) nestedMap` |  `struct MapKeyST0 {` <br>  &nbsp; `PubKeyHash key0;` <br> &nbsp; `PubKeyHash key1;` <br>`}` <br>`...` <br>`HashedMap<MapKeyST0, int> nestedMap` | *`nestedMap[addr1]` does not work, only `nestedMap[addr1][addr2]` works*
If statement | `if (a > 2) {` <br> &nbsp; `...` <br> `else if (a == 0) {` <br> &nbsp;  `...` <br> `} else {` <br> &nbsp; `...` <br> `}`| `if (a > 2) {` <br> &nbsp; `...` <br> `else if (a == 0) {` <br> &nbsp;  `...` <br> `} else {` <br> &nbsp; `...` <br> `}` |
For loop | `for (uint i = 0; i < 3; i++) {` <br>  &nbsp;  `...` <br> `}` | *Not Supported*   |  |
While loop | `while (a > 0) {` <br>  &nbsp;  `...` <br> `}`| *Not Supported*  |  |
Do-While loop | `do {` <br> &nbsp; `...` <br> `} while (a > 0);`| *Not Supported*  |  |
Assembly | `assembly {` <br> &nbsp; `...` <br> `}`| *Not Supported*  |  |
Return | *at the last statement without value*<br> `return ;` <br> *at the last statement with value* <br> `return a;` <br> *at the middle statement* <br> `function get(uint amount) public view returns (uint) {` <br>      &nbsp; `if (amount > 0)` <br> &nbsp; &nbsp; `return amount;` <br> &nbsp; `return 0;` <br>  `}` | *at the last statement without value*<br> `return true;` or *empty* <br>  *at the last statement with value* <br> `require(this.storedData == retVal);` or `return a;` <br> *at the middle statement* <br> `function get(int amount) : int {` <br>  &nbsp; `int ret = 0;` <br> &nbsp; `bool returned = false;` <br> &nbsp; `if (amount > 0) {` <br> &nbsp; &nbsp;`{` <br> &nbsp; &nbsp; &nbsp;`ret = amount;` <br> &nbsp;&nbsp;&nbsp;&nbsp; `returned = true;` <br> &nbsp;&nbsp; `}` <br> &nbsp;  `}` <br> &nbsp; `return returned ? ret : 0;` <br>  `}` | *return in sCrypt can only appear as the last statement of a function*
Break |`break;`| *Not Supported*  | |
Continue |`continue;`| *Not Supported*  | |
Assert | `assert(x > y);` |  `require(x > y);` |
Require | `require(x > y, "message");` |  `require(x > y);` | *message is ignored*
Error Definition | `error Unauthorized();`  |  *empty* |
Revert | `revert Unauthorized();` | `require(false);` |
Try Catch | `try ... {}` <br> `catch Error(...)` <br> `{ ... }` | *Not Supported* |  |
Units, global constants and type ranges | `1 ether`<br>`1 wei`<br>`1 gwei` <br>`1 seconds`<br>`1 minutes`<br>`1 hours`<br>`1 days`<br>`1 weeks`<br>`type(uint).min`<br>`type(uint).max`<br>`type(int8).min`<br>`type(uint8).max`<br>...<br> |  *Not Supported* |  |
Block and transaction properties| `blockhash(blockNumber)` <br>`block.coinbase` <br>`block.difficulty` <br>`block.gaslimit` <br>`block.number`<br>`block.timestamp`<br>`gasleft()` <br>`msg.data` <br>`msg.gas` <br>`msg.sig` <br>`tx.gasprice` <br>`tx.origin` <br>|*Not Supported* |  |
msg.sender | `msg.sender;` |  `PubKeyHash msgSender = hash160(pubKey);` <br> `require(checkSig(sig, pubKey));` | *will automatically add two parameters to the function signature： `Sig sig, PubKey pubKey`*
msg.value | `msg.value;` |   `SigHash.value(txPreimage);` | *will automatically add one parameters to the function signature： `SigHashPreimage txPreimage`*











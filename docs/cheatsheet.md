
# All supported features
<table border="1" style="width: 100%">

<colgroup>
    <col span="1" style="width: 10%;">
    <col span="1" style="width: 40%;">
    <col span="1" style="width: 40%;">
    <col span="1" style="width: 10%; word-break:break-all;">
</colgroup>
<thead>

<tr>
    <td><b>Feature</b></td>
    <td><b>Solidity</b></td>
    <td><b>sCrypt</b></td>
    <td><b>Note</b></td>
</tr>
</thead>
<tbody>
<tr>
    <td><b>Pragma Directive</b></td>
    <td><pre><code>pragma solidity ^0.8.10;
pragma experimental ABIEncoderV2;</code></pre></td>
    <td>empty</td>
    <td ></td>
</tr>

<tr>
    <td><b>Import Directive</b></td>
    <td><pre><code> import "./Foo.sol";</code></pre></td>
    <td><pre><code> import "./Foo.sol";</code></pre></td>
    <td >only support local file</td>
</tr>


<tr>
    <td><b>Type address</b></td>
    <td><pre><code> address</code></pre></td>
    <td><pre><code> PubKeyHash</code></pre></td>
    <td></td>
</tr>

<tr>
    <td><b>Type int</b></td>
    <td><pre><code> int , int8 , int16 , ... , int248 , int256 </code></pre></td>
    <td><pre><code> int</code></pre></td>
    <td></td>
</tr>

<tr>
    <td><b>Type uint</b></td>
    <td><pre><code> uint , uint8 , uint16 , ... , uint248 , uint256 </code></pre></td>
    <td><pre><code> int</code></pre></td>
    <td></td>
</tr>

<tr>
    <td><b>Type bytes</b></td>
    <td><pre><code> bytes , bytes1 , bytes2 , ... , bytes31 , bytes32 </code></pre></td>
    <td><pre><code> bytes</code></pre></td>
    <td></td>
</tr>

<tr>
    <td><b>Type bool</b></td>
    <td><pre><code> bool </code></pre></td>
    <td><pre><code> bool</code></pre></td>
    <td></td>
</tr>

<tr>
    <td><b>Type string</b></td>
    <td><pre><code> string </code></pre></td>
    <td><pre><code> bytes</code></pre></td>
    <td></td>
</tr>

<tr>
    <td><b>Type comment</b></td>
    <td><pre><code>// comment
/* comment */ </code></pre></td>
    <td><pre><code>empty</code></pre></td>
    <td></td>
</tr>

<tr>
    <td><b>Type Array</b></td>
    <td><pre><code>uint[]
uint[3]</code></pre></td>
    <td><pre><code>unsupported
uint[3]</code></pre></td>
    <td></td>
</tr>

<tr>
    <td><b>Boolean Literal</b></td>
    <td><pre><code> true, false</code></pre></td>
    <td><pre><code> true, false</code></pre></td>
    <td></td>
</tr>

<tr>
    <td><b>Number Literal</b></td>
    <td><pre><code>1
0x1</code></pre></td>
    <td><pre><code>1
0x1</code></pre></td>
    <td></td>
</tr>

<tr>
    <td><b>Hex Literal</b></td>
    <td><pre><code>hex"0101"
hex"ff00_0000_0000_0004"</code></pre></td>
    <td><pre><code>b'0101'
unsupported</code></pre></td>
    <td></td>
</tr>

<tr>
    <td><b>String Literal</b></td>
    <td><pre><code>"hello world"</code></pre></td>
    <td><pre><code>"hello world"</code></pre></td>
    <td></td>
</tr>


<tr >
    <td rowspan="5"><b>Array</b></td>
    <td><pre><code>// literal
[1, 2, 3]</code></pre> </td>
    <td><pre><code>[1, 2, 3]</code></pre> </td>
    <td></td>
</tr>

<tr>
    <td><pre><code>// length
s.length </code></pre> </td>
    <td><pre><code>unsupported </code></pre> </td>
    <td></td>
</tr>
<tr >
    <td><pre><code>// Index by number
a[1] </code></pre> </td>
    <td><pre><code>a[1]</code></pre> </td>
    <td></td>
</tr>
<tr >
    <td><pre><code>// Index by expression 
a[1 + i]</code></pre> </td>
    <td><pre><code>a[1 + i]</code></pre> </td>
    <td>only a <a href="https://scryptdoc.readthedocs.io/en/latest/ctc.html">compile-time constant</a> (CTC) can be used as an index when writing array in sCrypt</td>
</tr>
<tr >
    <td><pre><code>// push/pop element 
a.push(3);
a.pop(); </code></pre> </td>
    <td><pre><code>unsupported </code></pre> </td>
    <td></td>
</tr>
<tr>
    <td><b>Immutable variables</b></td>
    <td><pre><code>uint immutable x; </code></pre></td>
    <td><pre><code>const int x;</code></pre></td>
    <td></td>
</tr>

<tr>
    <td><b>Contract Define</b></td>
    <td><pre><code>contract Coin {
    ...
} </code></pre></td>
    <td><pre><code>contract Coin {
    ...
} </code></pre></td>
    <td></td>
</tr>

<tr>
    <td><b>Contract Property</b></td>
    <td><pre><code>contract Coin {
    address public minter; 
    ...
}</code></pre></td>
    <td><pre><code>contract Coin {
    @state public PubKeyHash minter;
    ...
} </code></pre></td>
    <td>cannot support property with initialization: <pre><code>uint amount = 1000;</code></pre></td>
</tr>

<tr>
    <td><b>Public function</b></td>
    <td><pre><code> function get() public view returns (uint) {
    return storedData;
} </code></pre></td>
    <td><pre><code> function get() : int {
    return this.storedData;
}</code></pre></td>
    <td></td>
</tr>

<tr>
    <td><b>External function</b></td>
    <td><pre><code>function set(uint x) external {
    storedData = x;
}</code></pre></td>
    <td><pre><code>public function set(int x, SigHashPreimage txPreimage) {
    this.storedData = x;
    require(this.propagateState(txPreimage));
}</code></pre></td>
    <td >automatically add require statement <pre><code>require(this.propagateState(txPreimage));</code></pre> and function paremeter <pre><code>SigHashPreimage txPreimage</code></pre> when transpiling external function</td>
</tr>

<tr>
    <td><b>Event</b></td>
    <td><pre><code>event Event(); </code></pre></td>
    <td><pre>empty</pre></td>
    <td></td>
</tr>

<tr>
    <td><b>Emit Event</b></td>
    <td><pre><code>emit Log(msg.sender, "Hello EVM!");</code></pre></td>
    <td><pre>empty</pre></td>
    <td></td>
</tr>

<tr>
    <td rowspan="2"><b>Mapping</b></td>
    <td><pre><code>mapping (address => uint)</code></pre></td>
    <td><pre><code>HashedMap<PubKeyHash, int></code></pre></td>
    <td><a href="https://scryptdoc.readthedocs.io/en/latest/contracts.html#library-hashedmap">HashedMap</a></td>
</tr>

<tr>
    <td><pre><code>mapping (address => mapping (address => uint)) nestedMap</code></pre></td>
    <td><pre><code>struct MapKeyST0 {
    PubKeyHash key0;
    PubKeyHash key1;
} 
...
HashedMap<MapKeyST0, int> nestedMap</code></pre></td>
    <td>nestedMap[addr1] does not work, only nestedMap[addr1][addr2] works</td>
</tr>

<tr>
    <td><b>If statement</b></td>
    <td><pre><code>if (a > 2) {
    ...
} else if (a == 0) {
    ...
} else {
    ...
}</code></pre></td>
    <td><pre><code>if (a > 2) {
    ...
} else if (a == 0) {
    ...
} else {
    ...
}</code></pre></td>
    <td></td>
</tr>

<tr>
    <td rowspan="3"><b>Return</b></td>
    <td>at the last statement without value: <br><pre><code>return ;</code></pre></td>
    <td><pre><code>return true;</code></pre></td>
    <td></td>
</tr>

<tr>
    <td>at the last statement with value: <br><pre><code>return a;</code></pre></td>
    <td><pre><code>require(this.storedData == retVal);</code></pre> or <pre><code> return a; </code></pre></td>
    <td></td>
</tr>

<tr>
    <td>at the middle statement: <br> <pre><code>function get(uint amount) public view returns (uint) {
    if (amount > 0)
        return amount;
    return 0;
}</code></pre></td>
    <td><pre><code>function get(int amount) : int {
    int ret = 0;
    bool returned = false;
    if (amount > 0) {
        {
            ret = amount;
            returned = true;
        }
    }
    return returned ? ret : 0;
}</code></pre></td>
    <td>return in sCrypt can only appear as the last statement of a function</td>
</tr>

<tr>
    <td ><b>Assert</b></td>
    <td><pre><code>assert(x > y);</code></pre></td>
    <td><pre><code>require(x > y);</code></pre></td>
    <td></td>
</tr>

<tr>
    <td ><b>Require</b></td>
    <td><pre><code>require(x > y, "message");</code></pre></td>
    <td><pre><code>require(x > y);</code></pre></td>
    <td>message is ignored</td>
</tr>

<tr>
    <td ><b>Error Definition</b></td>
    <td><pre><code>error Unauthorized();</code></pre></td>
    <td>empty</td>
    <td></td>
</tr>

<tr>
    <td ><b>Revert</b></td>
    <td><pre><code>revert Unauthorized();</code></pre></td>
    <td><pre><code>require(false);</code></pre></td>
    <td></td>
</tr>

<tr>
    <td ><b>msg.sender</b></td>
    <td><pre><code>msg.sender;</code></pre></td>
    <td><pre><code>PubKeyHash msgSender = hash160(pubKey);
require(checkSig(sig, pubKey));</code></pre></td>
    <td>will automatically add two parameters to the function signature： <pre><code>Sig sig, PubKey pubKey</code></pre></td>
</tr>

<tr>
    <td ><b>msg.value</b></td>
    <td><pre><code>msg.value;</code></pre></td>
    <td><pre><code>SigHash.value(txPreimage);</code></pre></td>
    <td>will automatically add one parameter to the function signature： <pre><code>SigHashPreimage txPreimage</code></pre></td>
</tr>

</tbody>
</table>

# All unsupported features 


<table border="1" style="width: 100%">


<colgroup>
    <col span="1" style="width: 10%;">
    <col span="1" style="width: 40%;">
    <col span="1" style="width: 40%;">
</colgroup>
<thead>

<tr>
    <td><b>Feature</b></td>
    <td><b>Solidity</b></td>
</tr>
</thead>
<tbody>

<tr>
    <td ><b>Type Fixed</b></td>
    <td><pre><code>fixed4x4</code></pre></td>
</tr>

<tr>
    <td ><b>Type Ufixed</b></td>
    <td><pre><code>ufixed4x4</code></pre></td>
</tr>

<tr>
    <td ><b>Parallel Assignment</b></td>
    <td><pre><code>(x, y) = (0, 1); </code></pre></td>
</tr>

<tr>
    <td ><b>Contract Creation</b></td>
    <td><pre><code>Contract c = new Contract(args);</code></pre></td>
</tr>
<tr>
    <td ><b>Interfaces</b></td>
    <td><pre><code>interface HelloWorld {
    ...
}</code></pre></td>
</tr>
<tr>
    <td ><b>Library</b></td>
    <td><pre><code>library HelloWorld {
    ...
}</code></pre></td>
</tr>
<tr>
    <td ><b>Inheritance</b></td>
    <td><pre><code>contract ERC20 is IERC20 {
    ...
}</code></pre></td>
</tr>

<tr>
    <td ><b>For loop </b></td>
    <td><pre><code>for (uint i = 0; i < 3; i++) {
    ...
}</code></pre></td>
</tr>

<tr>
    <td ><b>While loop </b></td>
    <td><pre><code>while (a > 0) {
    ...
}</code></pre></td>
</tr>

<tr>
    <td ><b>Do-While loop </b></td>
    <td><pre><code>do {
    ...
} while (a > 0);</code></pre></td>
</tr>

<tr>
    <td ><b>Assembly </b></td>
    <td><pre><code>assembly {
    ...
}</code></pre></td>
</tr>

<tr>
    <td ><b>Break </b></td>
    <td><pre><code>break; </code></pre></td>
</tr>

<tr>
    <td ><b>Continue </b></td>
    <td><pre><code>continue; </code></pre></td>
</tr>

<tr>
    <td ><b>Try Catch </b></td>
    <td><pre><code>try ... {
    ...
} catch Error(...) {
    ...
} </code></pre></td>
</tr>

<tr>
    <td ><b>Units, global constants and type ranges</b></td>
    <td><pre><code>1 ether
1 wei
1 gwei
1 seconds
1 minutes
1 hours
1 days
1 weeks
type(uint).min
type(uint).max
type(int8).min
type(uint8).max
...
</code></pre></td>
</tr>

<tr>
    <td ><b>Block and transaction properties</b></td>
    <td><pre><code>blockhash(blockNumber) 
block.coinbase 
block.difficulty
block.gaslimit
block.number
block.timestamp
gasleft()
msg.data
msg.gas
msg.sig
tx.gasprice
tx.origin
</code></pre></td>
</tr>



</tbody>
</table>









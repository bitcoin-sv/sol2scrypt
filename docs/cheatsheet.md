<style>

code {
  display: block;
  white-space: pre-wrap   
}

</style>
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
    <td><code> pragma solidity ^0.8.10; <br> pragma experimental ABIEncoderV2;</code></td>
    <td>empty</td>
    <td ></td>
</tr>

<tr>
    <td><b>Import Directive</b></td>
    <td><code> import "./Foo.sol";</code></td>
    <td><code> import "./Foo.sol";</code></td>
    <td >only support local file</td>
</tr>


<tr>
    <td><b>Type address</b></td>
    <td><code> address</code></td>
    <td><code> PubKeyHash</code></td>
    <td></td>
</tr>

<tr>
    <td><b>Type int</b></td>
    <td><code> int , int8 , int16 , ... , int248 , int256 </code></td>
    <td><code> int</code></td>
    <td></td>
</tr>

<tr>
    <td><b>Type uint</b></td>
    <td><code> uint , uint8 , uint16 , ... , uint248 , uint256 </code></td>
    <td><code> int</code></td>
    <td></td>
</tr>

<tr>
    <td><b>Type bytes</b></td>
    <td><code> bytes , bytes1 , bytes2 , ... , bytes31 , bytes32 </code></td>
    <td><code> bytes</code></td>
    <td></td>
</tr>

<tr>
    <td><b>Type bool</b></td>
    <td><code> bool </code></td>
    <td><code> bool</code></td>
    <td></td>
</tr>

<tr>
    <td><b>Type string</b></td>
    <td><code> string </code></td>
    <td><code> bytes</code></td>
    <td></td>
</tr>

<tr>
    <td><b>Type comment</b></td>
    <td><code> // comment <br> /* comment */ </code></td>
    <td><code> empty</code></td>
    <td></td>
</tr>

<tr>
    <td><b>Type Array</b></td>
    <td><code> uint[] <br> uint[3]</code></td>
    <td><code> unsupported <br> uint[3]</code></td>
    <td></td>
</tr>

<tr>
    <td><b>Boolean Literal</b></td>
    <td><code> true, false</code></td>
    <td><code> true, false</code></td>
    <td></td>
</tr>

<tr>
    <td><b>Number Literal</b></td>
    <td><code> 1 <br> 0x1</code></td>
    <td><code> 1 <br> 0x1</code></td>
    <td></td>
</tr>

<tr>
    <td><b>Hex Literal</b></td>
    <td><code> hex"0101" <br> hex"ff00_0000_0000_0004" </code></td>
    <td><code> `b'0101'` <br> unsupported</code></td>
    <td></td>
</tr>

<tr>
    <td><b>String Literal</b></td>
    <td><code> "hello world" </code></td>
    <td><code> "hello world" </code></td>
    <td></td>
</tr>


<tr >
    <td rowspan="5"><b>Array</b></td>
    <td><code>// 1. literal <br> [1, 2, 3] </code> </td>
    <td><code>[1, 2, 3] </code> </td>
    <td></td>
</tr>

<tr>
    <td><code>// 2. length <br> s.length </code> </td>
    <td><code>unsupported </code> </td>
    <td></td>
</tr>
<tr >
    <td><code>// 3. Index by number <br> a[1] </code> </td>
    <td><code>a[1] </code> </td>
    <td></td>
</tr>
<tr >
    <td><code>// 4. Index by expression <br> a[1 + i] </code> </td>
    <td><code>a[1 + i] </code> </td>
    <td>only a <a href="https://scryptdoc.readthedocs.io/en/latest/ctc.html">compile-time constant</a> (CTC) can be used as an index when writing array in sCrypt</td>
</tr>
<tr >
    <td><code>// 5. push/pop element <br> a.push(3)/a.pop() </code> </td>
    <td><code>unsupported </code> </td>
    <td></td>
</tr>
<tr>
    <td><b>Immutable variables</b></td>
    <td><code> uint immutable x; </code></td>
    <td><code> const int x;</code></td>
    <td></td>
</tr>

<tr>
    <td><b>Contract Define</b></td>
    <td><code> contract Coin { ...} </code></td>
    <td><code> contract Coin { ...} </code></td>
    <td></td>
</tr>

<tr>
    <td><b>Contract Property</b></td>
    <td><code> contract Coin { <br > &nbsp; address public minter; <br> &nbsp; ... <br> } </code></td>
    <td><code> contract Coin { <br > &nbsp; @state public PubKeyHash minter; <br> &nbsp; ... <br> } </code></td>
    <td>cannot support property with initialization: <code>uint amount = 1000;</code></td>
</tr>

<tr>
    <td><b>Public function</b></td>
    <td><code> function get() public view returns (uint) { <br> &nbsp; return storedData; <br> } </code></td>
    <td><code> function get() : int { <br> &nbsp; return this.storedData; <br> }</code></td>
    <td></td>
</tr>

<tr>
    <td><b>External function</b></td>
    <td><code> function set(uint x) external { <br> &nbsp; storedData = x; <br> } </code></td>
    <td><code> public function set(int x, SigHashPreimage txPreimage) { <br>  &nbsp; this.storedData = x; <br> &nbsp; require(this.propagateState(txPreimage)); <br>  }</code></td>
    <td  >automatically add require statement <code>require(this.propagateState(txPreimage));</code> and function paremeter <code>SigHashPreimage txPreimage</code> when transpiling external function</td>
</tr>

<tr>
    <td><b>Event</b></td>
    <td><code>event Event(); </code></td>
    <td><code> empty</code></td>
    <td></td>
</tr>

<tr>
    <td><b>Emit Event</b></td>
    <td><code>emit Log(msg.sender, "Hello EVM!");</code></td>
    <td><code> empty</code></td>
    <td></td>
</tr>

<tr>
    <td rowspan="2"><b>Mapping</b></td>
    <td><code>mapping (address => uint)</code></td>
    <td><code> HashedMap<PubKeyHash, int></code></td>
    <td><a href="https://scryptdoc.readthedocs.io/en/latest/contracts.html#library-hashedmap">HashedMap </a></td>
</tr>

<tr>
    <td><code>mapping (address => mapping (address => uint)) nestedMap</code></td>
    <td><code>struct MapKeyST0 { <br>  &nbsp; PubKeyHash key0; <br> &nbsp; PubKeyHash key1; <br> } <br> ... <br> HashedMap<MapKeyST0, int> nestedMap</code></td>
    <td>nestedMap[addr1] does not work, only nestedMap[addr1][addr2] works</td>
</tr>

<tr>
    <td><b>If statement</b></td>
    <td><code>if (a > 2) { <br> &nbsp; ... <br> else if (a == 0) { <br> &nbsp; ... <br> } else { <br> &nbsp; ... <br> }</code></td>
    <td><code>if (a > 2) { <br> &nbsp; ... <br> else if (a == 0) { <br> &nbsp; ... <br> } else { <br> &nbsp; ... <br> }</code></td>
    <td></td>
</tr>

<tr>
    <td rowspan="3"><b>Return</b></td>
    <td> at the last statement without value <br> <code>return ;</code></td>
    <td><code>return true;</code> <br> or <br> empty</td>
    <td></td>
</tr>

<tr>
    <td> at the last statement with value <br> <code>return a;</code></td>
    <td><code>require(this.storedData == retVal);</code> <br> or <br> <code> return a; </code></td>
    <td></td>
</tr>

<tr>
    <td> at the middle statement <br> <code>function get(uint amount) public view returns (uint) { <br> &nbsp; if (amount > 0) <br> &nbsp; &nbsp; return amount; <br> &nbsp; return 0; <br> }</code></td>
    <td><code>function get(int amount) : int { <br>  &nbsp; int ret = 0; <br> &nbsp; bool returned = false; <br> &nbsp; if (amount > 0) { <br> &nbsp; &nbsp; { <br> &nbsp; &nbsp; &nbsp; ret = amount; <br> &nbsp;&nbsp;&nbsp;&nbsp; returned = true; <br> &nbsp;&nbsp; } <br> &nbsp; } <br> &nbsp; return returned ? ret : 0; <br> } </code></td>
    <td>return in sCrypt can only appear as the last statement of a function</td>
</tr>

<tr>
    <td ><b>Assert</b></td>
    <td><code>assert(x > y);</code></td>
    <td><code>require(x > y);</code></td>
    <td></td>
</tr>

<tr>
    <td ><b>Require</b></td>
    <td><code>require(x > y, "message");</code></td>
    <td><code>require(x > y);</code></td>
    <td>message is ignored</td>
</tr>

<tr>
    <td ><b>Error Definition</b></td>
    <td><code>error Unauthorized();</code></td>
    <td>empty</td>
    <td></td>
</tr>

<tr>
    <td ><b>Revert</b></td>
    <td><code>revert Unauthorized();</code></td>
    <td><code>require(false);</code></td>
    <td></td>
</tr>

<tr>
    <td ><b>msg.sender</b></td>
    <td><code>msg.sender;</code></td>
    <td><code>PubKeyHash msgSender = hash160(pubKey); <br> require(checkSig(sig, pubKey)); </code></td>
    <td>will automatically add two parameters to the function signature： <code>Sig sig, PubKey pubKey</code></td>
</tr>

<tr>
    <td ><b>msg.value</b></td>
    <td><code>msg.value;</code></td>
    <td><code>SigHash.value(txPreimage);</code></td>
    <td>will automatically add one parameter to the function signature： <code>SigHashPreimage txPreimage</code></td>
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
    <td><code>fixed4x4</code></td>
</tr>

<tr>
    <td ><b>Type Ufixed</b></td>
    <td><code>ufixed4x4</code></td>
</tr>

<tr>
    <td ><b>Parallel Assignment</b></td>
    <td><code>(x, y) = (0, 1); </code></td>
</tr>

<tr>
    <td ><b>Contract Creation</b></td>
    <td><code>Contract c = new Contract(args);</code></td>
</tr>
<tr>
    <td ><b>Interfaces</b></td>
    <td><code>interface HelloWorld { <br> &nbsp; ... <br> }</code></td>
</tr>
<tr>
    <td ><b>Library</b></td>
    <td><code>library HelloWorld { <br> &nbsp; ... <br> }</code></td>
</tr>
<tr>
    <td ><b>Inheritance</b></td>
    <td><code>contract ERC20 is IERC20 { <br> &nbsp; ... <br> }</code></td>
</tr>

<tr>
    <td ><b>For loop </b></td>
    <td><code>for (uint i = 0; i < 3; i++) { <br>  &nbsp; ... <br> }</code></td>
</tr>

<tr>
    <td ><b>While loop </b></td>
    <td><code>while (a > 0) { <br>  &nbsp; ... <br> }</code></td>
</tr>

<tr>
    <td ><b>Do-While loop </b></td>
    <td><code>do { <br> &nbsp; ... <br> } while (a > 0);</code></td>
</tr>

<tr>
    <td ><b>Assembly </b></td>
    <td><code>assembly { <br> &nbsp; ... <br> }</code></td>
</tr>

<tr>
    <td ><b>Break </b></td>
    <td><code>break; </code></td>
</tr>

<tr>
    <td ><b>Continue </b></td>
    <td><code>continue; </code></td>
</tr>

<tr>
    <td ><b>Try Catch </b></td>
    <td><code>try ... {} <br> catch Error(...) <br> { ... } </code></td>
</tr>

<tr>
    <td ><b>Units, global constants and type ranges</b></td>
    <td><code>1 ether<br>1 wei<br>1 gwei <br>1 seconds<br>1 minutes<br>1 hours<br>1 days<br>1 weeks<br>type(uint).min<br>type(uint).max<br>type(int8).min<br>type(uint8).max<br>...<br></code></td>
</tr>

<tr>
    <td ><b>Block and transaction properties</b></td>
    <td><code>
blockhash(blockNumber) 
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
</code></td>
</tr>



</tbody>
</table>









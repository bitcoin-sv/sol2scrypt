
# All supported features
<table border="1" >

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
    <td><code>address.balance</code> is not currently supported</td>
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
    <td><pre><code>string</code></pre></td>
    <td><pre><code>bytes</code></pre></td>
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
    <td></td>
</tr>
<tr >
    <td><pre><code>// push/pop element 
a.push(3);
a.pop(); </code></pre> </td>
    <td><pre><code>unsupported </code></pre> </td>
    <td></td>
</tr>

<tr>
    <td rowspan="3"><b>Operator</b></td>
    <td>Unary <pre><code>-, (), ++, --, ! </code></pre></td>
    <td><pre><code>-, (), ++, --, !, ~ </code></pre></td>
    <td>since bitcoin is little endian, the result of bitwise <code>~</code> is different from solidity</td>
</tr>

<tr>
    <td>Binary <pre><code>+, -, *, /, %, +=, -=, *=, /=, %=, ==, !=, <, <=, >, >=, &&, ||</code></pre></td>
    <td><pre><code>+, -, *, /, %, +=, -=, *=, /=, %=, ==, !=, <, <=, >, >=, &&, ||, &, |, ^, &=, |=, ^=, <<=, >>=, <<, >></code></pre></td>
    <td>for these <code>&, |, ^, &=, |=, ^=</code> operators, if it is a positive integer, the result of the operation is usually consistent with solidity. If it contains negative numbers, the operation result may not be consistent with solidity.</td>
</tr>

<tr>
    <td>Ternary <pre><code>a > b ? a : b</code></pre></td>
    <td><pre><code>a > b ? a : b</code></pre></td>
    <td></td>
</tr>

<tr>
    <td><b>Constant variables</b></td>
    <td><pre><code>uint constant x = 1;</code></pre></td>
    <td><pre><code>static const int x = 1;</code></pre></td>
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
    <td rowspan="4"><b>Access Modifier</b></td>
    <td><pre><code>private</code></pre></td>
    <td><pre><code>private</code></pre></td>
    <td></td>
</tr>

<tr >
    <td><pre><code>public</code></pre></td>
    <td><pre><code>public</code></pre></td>
    <td></td>
</tr>
<tr >
    <td><pre><code>internal</code></pre></td>
    <td><pre><code>default</code></pre></td>
    <td></td>
</tr>
<tr >
    <td><pre><code>external</code></pre></td>
    <td><pre><code>public</code></pre></td>
    <td></td>
</tr>


<tr>
    <td><b>Internal function</b></td>
    <td><pre><code> function get() internal view returns (uint) {
    return storedData;
} </code></pre></td>
    <td><pre><code>function get() : int {
    return this.storedData;
}</code></pre></td>
    <td>no recursion allowed</td>
</tr>

<tr>
    <td><b>Private function</b></td>
    <td><pre><code>function get() private view returns (uint) {
    return storedData;
} </code></pre></td>
    <td><pre><code>private function get() : int {
    return this.storedData;
}</code></pre></td>
    <td>no recursion allowed</td>
</tr>

<tr>
    <td><b>Public and External function</b></td>
    <td><pre><code>function set(uint x) external {
    storedData = x;
}</code></pre></td>
    <td><pre><code>public function set(int x, SigHashPreimage txPreimage) {
    this.storedData = x;
    require(this.propagateState(txPreimage));
}</code></pre></td>
    <td >automatically add require statement <pre><code>require(this.propagateState(txPreimage));</code></pre> and function paremeter <pre><code>SigHashPreimage txPreimage</code></pre> when transpiling public and external function</td>
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
    <td>See <a href="https://scryptdoc.readthedocs.io/en/latest/contracts.html#library-hashedmap">HashedMap</a> <br>
some limitations: <br>
1: dynamic modification of the key value is not supported, such as the following code: <br><pre><code>
map[a] = 1;
a++;
map[a] = 2;
</code></pre>
2: only supports using mapping in public and external function.
</td>
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
    <td><b>For Loop</b></td>
    <td><pre><code>for(uint i=0; i<2; i++) {
    sum += i;
    if(i > 10)
        break;
}</code></pre></td>
    <td><pre><code>bool loopBreakFlag0 = false;
int i = 0;
loop (__LoopCount__0) {
    if (!loopBreakFlag0 && i < 2) {
        sum += i;
        if (i > 10)
            loopBreakFlag0 = true;
        i++;
    }
}</code></pre></td>
    <td>we do not support unbounded loop.</td>
</tr>


<tr>
    <td><b>While Loop</b></td>
    <td><pre><code>uint i = 0;
uint sum = 0;
while(i <10) {
    sum += i;
    if (sum > 100) {
        break;
    }
    i++;
}</code></pre></td>
    <td><pre><code>int i = 0;
int sum = 0;
bool loopBreakFlag0 = false;
loop (__LoopCount__0) {
    if (!loopBreakFlag0 && i < 10) {
        sum += i;
        if (sum > 100) {
            loopBreakFlag0 = true;
        }
        if(!loopBreakFlag0) {
            i++;
        }
    }
}</code></pre></td>
    <td></td>
</tr>

<tr>
   <td><b>Do While Loop</b></td>
    <td><pre><code>uint i = 0;
uint sum = 0;
do {
    sum += i;
    if (sum < 20)
        continue; 
    i++;
} while (i < 100);</code></pre></td>
    <td><pre><code>int i = 0;
int sum = 0;
{
    sum += i;
    i++;
}
loop (__LoopCount__0) {
    if (i < 100) {
        bool loopContinueFlag0 = false;
        sum += i;
        if (sum < 20) {
            {
                loopContinueFlag0 = true;
            }
        }
        if (!loopContinueFlag0) {
            i++;
        }
    }
}</code></pre></td>
    <td></td>
</tr>


<tr>
    <td ><b>Struct</b></td>
    <td><pre><code>struct Todo {
    string text;
    bool completed; 
}</code></pre></td>
    <td><pre><code>struct Todo {
    bytes text;
    bool completed; 
}</code></pre></td>
    <td>Structures with the same name is not currently supported</td>
</tr>

<tr>
    <td ><b>Library</b></td>
    <td><pre><code>library SafeMath {
    function add(uint x, uint y) internal pure returns (uint) {
        uint z = x + y;
        require(z >= x, "uint overflow");
        return z;
    }
}</code></pre></td>
    <td><pre><code>library SafeMath {
  static function add(int x, int y) : int {
    int z = x + y;
    require(z >= x);
    return z;
  }
}</code></pre></td>
    <td>1: Library cannot be independently deployed<br>
2: Library function will be transpiled into static function</td>
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

# Currently unsupported features 


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
    <td ><b>Power Operator</b></td>
    <td><pre><code>x ** n </code></pre></td>
</tr>

<tr>
    <td ><b>Parallel Assignment</b></td>
    <td><pre><code>(x, y) = (0, 1); </code></pre></td>
</tr>

<tr>
    <td ><b>payable</b></td>
    <td><pre>function deposit() public payable {} </code></pre></td>
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
    <td ><b>Inheritance</b></td>
    <td><pre><code>contract ERC20 is IERC20 {
    ...
}</code></pre></td>
</tr>

<tr>
    <td ><b>Assembly </b></td>
    <td><pre><code>assembly {
    ...
}</code></pre></td>
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

<tr>
    <td ><b>Receive and Fallback Function</b></td>
    <td><pre><code>receive() external payable {}
fallback() external payable {}</code></pre></code></td>
</tr>


<tr>
    <td ><b>address.balance</b></td>
    <td><pre><code>address(this).balance;</code></pre></code></td>
</tr>

<tr>
    <td ><b>address.transfer</b></td>
    <td><pre><code>payable(owner).transfer(msg.value)</code></pre></code></td>
</tr>

<tr>
    <td ><b>address.send</b></td>
    <td><pre><code>_to.send(msg.value);</code></pre></code></td>
</tr>

<tr>
    <td ><b>address.call</b></td>
    <td><pre><code>_to.call{value: msg.value}("data");</code></pre></code></td>
</tr>

<tr>
    <td ><b>Calling Other Contract</b></td>
    <td><pre><code>function setXandSendEther(Callee _callee, uint _x) public payable {
    (uint x, uint value) = _callee.setXandSendEther{value: msg.value}(_x);
}</code></pre></code></td>
</tr>


<tr>
    <td ><b>Buildin Function</b></td>
    <td><pre><code>keccak256();
ecrecover();
addmod();
mulmod();
selfdestruct();
</code></pre></code></td>
</tr>


<tr>
    <td ><b>Enum</b></td>
    <td><pre><code>enum FreshJuiceSize{ SMALL, MEDIUM, LARGE }</code></pre></code></td>
</tr>

<tr>
    <td ><b>Access Modifier</b></td>
    <td><pre><code>onlySeller</code></pre></code></td>
</tr>








</tbody>
</table>









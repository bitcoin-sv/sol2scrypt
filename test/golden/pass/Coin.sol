pragma solidity ^0.8.10;

contract Coin {
    // The keyword "public" makes those variables
    // readable from outside.
    address public minter;
    mapping (address => uint) public balances;

    mapping (address => bool) public b1;
    mapping (address => bytes) public b2;

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
        b1[receiver] != true;
        b2[receiver] = hex"0001";
    }

    function send(address receiver, uint amount, bool a, bytes memory b) external {
        if (balances[msg.sender] < amount) return;
        balances[msg.sender] -= amount;
        balances[receiver] += amount;
        b1[receiver] != a;
        b2[receiver] = b;
        emit Sent(msg.sender, receiver, amount);
    }
}
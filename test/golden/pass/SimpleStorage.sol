pragma solidity ^0.8.10;

contract SimpleStorage {
    uint storedData;
 
    uint immutable decimals;

    constructor() {
        decimals = 3;
    }
    function set(uint x) external {
        storedData = x;
    }

    function get() public view returns (uint) {
        return storedData;
    }
}
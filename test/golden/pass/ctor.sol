// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract Ctor {
    uint storedData;

    address addr;

    uint initBalance;

     constructor() payable {
        storedData = 0;
        addr = msg.sender;
        storedData = msg.value;
    }

    function set(uint x) payable external {
        storedData = x + msg.value;
    }

    function get() public view returns (uint) {
        return storedData;
    }

    function checkInitBalance() public view returns (uint) {
        return 1;
    }

    function propagateState() external {
        uint contractBalance = msg.value;
        uint msgValue = 15;
    }
}
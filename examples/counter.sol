// adapted from https://solidity-by-example.org/first-app/

// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract Counter {
    uint public count;

    // Function to get the current count
    function get() public view returns (uint) {
        return count;
    }

    // Function to increment count by 1
    function inc() public {
        count += 1;
    }

    // from https://solidity-by-example.org/state-variables/
    // You need to send a transaction to write to a state variable.
    function set(uint _count) public {
        count = _count;
    }
}

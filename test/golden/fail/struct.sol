// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

struct ST {
    uint8 a;
}
    
contract A {
    struct ST {
        uint8 a;
    }
}

contract B {
    struct ST {
        bool a;
        uint256 x;
    }
}

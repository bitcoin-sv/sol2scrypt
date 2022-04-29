// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract Mapping {
    mapping(address => uint) public myMap;
    
    function get(address addr) external {
        myMap[addr]++;
    }
    
    // function set(address addr, uint x) external {
    //     myMap[addr] = x;
    // }
}

// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract Mapping {

    uint i = 11;

    mapping(address => uint) public myMap;
    mapping(uint => address) public map1;
    
    function get(address _addr) external view returns (uint) {
        return myMap[_addr];
    }

    function get2(address _addr) internal view returns (uint) {
        return myMap[_addr];
    }
    
    function set(address _addr, uint _i) external {
        myMap[_addr] = _i;
    }

    function update(uint a, address _addr) external {
        map1[a] = _addr;
        a++;
        map1[a] = _addr;
    }

}
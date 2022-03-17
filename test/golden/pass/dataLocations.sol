// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract DataLocations {
    uint[3] public arr;
    mapping(uint => address) map;
    struct MyStruct {
        uint foo;
    }
    mapping(uint => MyStruct) myStructs;

    function _f(
        uint[3] storage _arr,
        mapping(uint => address) storage _map,
        MyStruct storage _myStruct
    ) internal {
        // do something with storage variables
    }

    function f() external {
        // call _f with state variables
        _f(arr, map, myStructs[1]);

        // get a struct from a mapping
        MyStruct storage myStruct = myStructs[1];
        // create a struct in memory
        MyStruct memory myMemStruct = MyStruct(0);
    }


    // You can return memory variables
    function g(uint[3] memory _arr) public returns (uint[3] memory) {
        // do something with memory array
    }

    function h(uint[3] calldata _arr) external {
        // do something with calldata array
    }
}
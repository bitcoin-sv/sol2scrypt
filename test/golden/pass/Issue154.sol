pragma solidity ^0.8.10;

struct Todo {
    string text;
    bool completed;
}

contract SimpleStorage {
    uint storedData;

    function get() public view returns (uint) {

        uint a;
        uint8 b;
        uint16 c;
        int d;
        int8 e;
        uint8[3] memory aa;


        bytes memory b1;
        bytes1 b2;
        bytes10 b3;
        bytes10[3] memory b3a;

        address a1;
        address[3] memory a1a;

        bool bb;
        bool[3] memory bba;

        Todo memory todo;

        Todo[3] memory todos;

        return storedData;
    }
}
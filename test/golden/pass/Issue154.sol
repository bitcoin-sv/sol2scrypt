pragma solidity ^0.8.10;

struct Todo {
    string text;
    bool completed;
}

contract SimpleStorage {
    uint storedData;

    function get() external view returns (uint) {

        uint a;
        uint8 b;
        uint16 c;
        int d;
        int8 e;
        uint8[3] memory aa;

        return storedData;
    }

    function get1() public view returns (bytes10[3] memory) {

        bytes calldata b1;
        bytes1 b2;
        bytes10 b3;
        bytes10[3] memory b3a;
        return b3a;
    }

    function get2() public view returns (Todo[3] memory) {

        address a1;
        address[3] calldata a1a;

        bool bb;
        bool[3] storage bba;

        Todo memory todo;

        Todo[3] memory todos;
        return todos;
    }
}
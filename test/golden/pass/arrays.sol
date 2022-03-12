pragma solidity ^0.8.10;

struct Todo {
    string text;
    bool completed;
}

contract Arrays {
    uint storedData;
    uint8[2][3] c;
    Todo[1][2][3][1] todos;
    function get() public view returns (Todo[1][2][3][1] memory) {

        //uint8[2][3] memory ccc =[[1,2],[3,4],[5,6]];
        uint8[1][2][3][1] memory aa =[[[[12],[34]],[[12],[34]], [[12],[34]]]];

        Todo[1][2][3][1] memory todos;

        bytes[1][2][3] memory bb;

        bool[3][4] memory bools = [[false, false, false], [false, false, false], [false, false, false], [false, false, false]];

        uint8[2][3] memory cc ;
        return todos;
    }
}
pragma solidity ^0.8.10;

struct A{
    string text;
}

struct B{
    string text;
    A[2][3][1] a;
}

struct L {
    B[2][2] b;
    string text;
    bool completed; 
}

struct Todo {
    string text;
    bool completed;
}

contract Arrays {
    uint8[2][3] c;
    Todo[2][3][4][5] todos;
    uint constant __LoopCount__0 = 1;
    uint constant __LoopCount__1 = 1;
    function get() external view returns (Todo[2][3][4][5] memory) {

        uint8[2][3][4][5] memory aa = 
            [
            [[[12, 22],[34, 3],[12, 22]],[[12, 22],[34, 3],[12, 22]],[[12, 22],[34, 3],[12, 22]],[[12, 22],[34, 3],[12, 22]]],
            [[[12, 22],[34, 3],[12, 22]],[[12, 22],[34, 3],[12, 22]],[[12, 22],[34, 3],[12, 22]],[[12, 22],[34, 3],[12, 22]]],
            [[[12, 22],[34, 3],[12, 22]],[[12, 22],[34, 3],[12, 22]],[[12, 22],[34, 3],[12, 22]],[[12, 22],[34, 3],[12, 22]]],
            [[[12, 22],[34, 3],[12, 22]],[[12, 22],[34, 3],[12, 22]],[[12, 22],[34, 3],[12, 22]],[[12, 22],[34, 3],[12, 22]]],
            [[[12, 22],[34, 3],[12, 22]],[[12, 22],[34, 3],[12, 22]],[[12, 22],[34, 3],[12, 22]],[[12, 22],[34, 3],[12, 22]]]
            ];
        Todo[2][3][4][5] memory todos;

        bytes[1][2][3] memory bb;

        bool[3][4] memory bools = [[false, false, false], [false, false, false], [false, false, false], [false, false, false]];

        uint8[2][3] memory cc ;
        return todos;
    }


    function getByIndex(uint k, uint x, L memory l) public view  {

        uint8[2][3] memory aa = [[1,2], [1,2], [1,2]];

        for(uint i=0; i<3; i++) {
            for(uint j=0; j<2; j++) {
                aa[i*1+k][j*1+k] = 33;
            }
        }

        l.b[k][x].a[k][k][x] = A({text: "aaa"});
        l.b[k][x].a[k][k][x].text = "aaa";
        l.b[k][x].a[k][0][x*k].text = "aaa";

        l.b[this.f1(k)][this.f1(k)].a[aa[k][k]][0][x*k].text = "aaa";

    }


    function f1(uint k) public pure returns (uint)  {
        return k;
    }
}
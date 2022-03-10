pragma solidity ^0.8.10;

contract SolidityTest {
    //solidity supports bitwise on number, but sCrypt supports bitwise on bytes
    function bitwise(uint16 a, uint16 b) public {
        uint16  and = a & b;

        uint16  or = a | b;

        uint16  xor = a ^ b;

        uint16  leftshift = a << b;

        uint16  rightshift = a >> b;

        a >>= 3;
        a |= 3;
        a ^= 3;
        a &= 3;
        b <<= 3;

        1 >> 1;

        1 << 1;

        uint16 not = ~a; 
    }

    function arithmetic(uint16 a, uint16 b) public {
        int128 ff = -2 ** 127 ;
    }

}

pragma solidity ^0.8.10;

contract SolidityTest {
    function arithmetic() public {
        uint16 a = 20;

        uint16 b = 10;


        uint256 sum = a + b;
        sum += a;

        uint256 diff = a - b;
        sum -= a;

        uint256 mul = a * b;
        sum *= a;

        uint256 div = a / b;
        sum /= a;

        uint256 mod = a % b;
        sum %= a;

        uint256 dec = --b;
        b--;

        uint256 inc = ++a;
        a++;

        bool eq = a == b;
        bool noteq = a != b;
        bool gtr = a > b;
        bool les = a < b;
        bool gtreq = a >= b;
        bool leseq = a <= b;


    }

    function ternary(uint16 a, uint16 b) public {
        uint result = (a > b ? a : b);
    }

    function logical(bool a, bool b) public {
        bool and = a && b;
        bool or = a || b;
        bool not = !a;
    }

    //only works on fixed bytes
    function bitwise(bytes1  a, bytes1 b) public {
        bytes1 and = a & b;
        and &= a;

        and & hex"01";

        bytes1  or = a | b;
        or |= a;

        or | hex"01";

        bytes1 xor = a ^ b;
        xor ^= a;

        xor ^ hex"01";
        
        bytes1  leftshift = a << 1;

        bytes1  rightshift = a >> 1;

        rightshift >>= 1;

        leftshift <<= 1;

        bytes1  not = ~a;

    }

}

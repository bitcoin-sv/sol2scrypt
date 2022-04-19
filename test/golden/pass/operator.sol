pragma solidity ^0.8.10;

contract SolidityTest {
    uint8 a;
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

        f1(a) >> 3;

        a << 3;
        a << f1(1);

        a >>= 3;

        a <<= 3;

        bool eq = a == b;
        bool noteq = a != b;
        bool gtr = a > b;
        bool les = a < b;
        bool gtreq = a >= b;
        bool leseq = a <= b;


    }

    function shift() public {
        uint256 a = 20;
        uint256 b = 10;

        a << b;

        b >> b;

        uint256 c = a <<= a <<= 1;

        c = a * 1 >> b - 1 << c + 1;

        a <<= a + 1;

        uint256 d = a <<= (a + 1) * 1 + (b - 1);

        uint8[3] memory aa = [0,0,0];

        aa[a>>1] = aa[a<<= 1 + 1];

        aa[a>>=1] = aa[a>> 1 + 1];

        f1(a >> 3) * (a >>= 1) << 3 >> (a + 1);


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

        uint8 x = 0; 

        uint8 y = 0;

        x &= x & y | x ^ x;

        y ^= ~x & y & x ^ x;

        y |= ~x & y & x | f2(x);

        uint8[3] memory aa = [0,0,0];

        aa[ ~x & y & x | f2(x)] = 3;

        aa[ ~x ^ f2(x) & x | f2(x)] = 3;

    }

    function unlock(uint ab) external {
        require(a == ab);
    }

    function f1(uint x) public pure returns (uint) {
        return x + 1;
    }

    function f2(uint8 x) public pure returns (uint8) {
        return x + 1;
    }


}

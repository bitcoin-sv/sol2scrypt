// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

library SafeMath {
    function add(uint x, uint y) internal pure returns (uint) {
        uint z = x + y;
        require(z >= x, "uint overflow");

        return z;
    }
}

library Math {
    uint constant public MAX_UINT = 2*256 - 1;

    function sqrt(uint y) internal pure returns (uint z) {
        if (y > 3) {
            z = y;
            uint x = y / 2 + 1;

        } else if (y != 0) {
            z = 1;
        }
    }
}

contract TestSafeMath {

    function testAdd(uint x, uint y) public pure returns (uint) {
        return SafeMath.add(x, y);
    }

    function testSquareRoot(uint x) public pure returns (uint) {
        return Math.sqrt(x);
    }
}


library Array {
    function remove(uint[9] storage arr, uint index) public {

    }
}

contract TestArray {


    uint[9] public arr ;

    function testArrayRemove() public {


        Array.remove(arr, 1);


    }
}

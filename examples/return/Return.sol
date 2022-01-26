pragma solidity ^0.8.10;

contract Return {


    function test_( uint amount) public view returns (uint) {
        uint x = 3;
        if(x == 0) {
            return 3;
        }

        x = x + amount;
        return x;
    }


    function test( uint amount) public view returns (uint) {
        uint x = 3;
        if(x == 0) {
            return x;
        }

        x = x + amount;
        return x;
    }

    function test1( uint amount) public returns (uint) {
        uint x = 3;
        if(x == 0) {
            return x;
        } else {
            return x++;
        }

        x = x + amount;
        return x;
    }

    function test2( uint amount) public returns (uint)  {
        uint x = 3;
        if(x > 0) {
            if(x == 3) {
                return x;
            }
            x++;
        } else {
            return x;
        }

        x = x + amount;
        return x;
    }

    function test3( uint amount) public returns (uint) {
        uint x = 3;
        if(x > 1) {
            if(x == 3) {
                return x;
            }
            x++;
        } else {
            if(x == 0) {
                return x;
            }
            x++;
        }

        x = x + amount;
        return x;
    }

}
pragma solidity ^0.8.10;

contract Return {


    // test return only in if
    function test0( uint amount) public view returns (uint) {
        uint x = 3;
        if(x == 0) {
            x++;
            return x;
        }

        x = x + amount;
        return x;
    }

    // test return both in if and else
    function test1( uint amount) public returns (uint) {
        uint x = 3;
        if(x == 3) {
            return x;
        } else {
            return x++;
        }
    }

     // test return in nested if
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

    // solidity
    function test3( uint amount, uint y) public returns (uint) {
        uint x = 3;
        if(x > 0) {
            if (x > 1) {
                x /= 2;
                if(x == 2) {
                    return x;
                }
                x--;
                if(x == 3) {
                    return x;
                } else {
                    x += y;
                }
                x += 2 * amount + 1;
            }
            x++;
            x += 11;
        } else {
            
            --x;
        }

        x = x + amount;
        x += 20 / amount - 12;
        return x;
    }

    // return only in else
    function test4( uint amount, uint y) public returns (uint) {
        uint x = 3;
        if(x > 0) {
            x++;
        } else {
            --x;
            return x;
        }

        x = x + amount;
        x += 20 / amount - 12;
        return x;
    }



    function test5( uint amount, uint y) public returns (uint) {
        uint x = 3;
        if(x > 0) {
            x++;
            if(x == 3) {
                return x;
            } else if(x == 5) {
                x++;
            } else if(x == 2) {
                x--;
            }
            x += 3;
        } else {
            --x;
        }

        x = x + amount;
        x += 20 / amount - 12;
        return x;
    }

}
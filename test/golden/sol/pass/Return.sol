pragma solidity ^0.8.10;

contract Return {
    uint storedData;

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
            return x;
        } else {
            --x;
        }

        x = x + amount;
        x += 20 / amount - 12;
        return x;
    }


    // test return no in a block
    function test6( uint x) public view returns (bool) {
        if(x == 0) 
            return true;
        
        return false;
    }


    // test return no in a block
    function test7( uint x) public view returns (bytes8) {
        if(x == 0) {
            if(x > 1){ 
                if(x > 9) {
                    return hex"ff00";
                }
                return hex"ff00";
            }
            return hex"ff00";
        }

        return hex"ff00";
    }

    //  no return at the end.
    function test8(uint x) public view returns (uint) {
        uint y = 1;
        if(x == 0) {
            if(x > 1){ 
                if(x > 9) {
                    return ((x * x + y) > x*y) ? x-- : ((y*x) -9) ;
                } else {
                    x = x*9- y + (x*y/100);
                }
                return (x-- - 200) * y  ;
            }
            return x -9;
        }
        x++;
    }


    //  no return at the end.
    function test9(uint x) public pure returns (uint) {
        uint y = 1;
        
    }

    function set(uint x) external {
        storedData++;
        uint a = 3;
        if(x > a) {
            return;
        }

        storedData = x;
    }

    function set1(uint x) public {

        uint a = 3;
        if(x > a) {
            return;
        }
        a++;
    }

    function set2(uint x) external returns (uint) {
        storedData++;
        uint a = 3;
        if(x > a) {
            return storedData;
        }

        storedData = x;
        return storedData;
    }

    function set3(uint x) external view returns (uint y) { y = x; }

    function foo() private {
        if (true) return;
        int a = 1;
    }

    function foo1() private {
        return;
    }

    function foo2() external {
        return;
    }

    function get() external payable { return ; }

    function get1() external view returns (uint) { return storedData; }

    function get2() external pure returns (uint) { return 1 + 1; }

    function get3() external view returns (uint) { {true; {true;} } return storedData; }


}
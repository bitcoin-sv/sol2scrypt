pragma solidity ^0.8.10;

contract Return {
    int counter;

    function test(uint x) public returns (int) {
        if (x > 0) {
            return -1;
        }

        counter++; // <--- this would NOT run if -1 is returned
        return 1;
    }

    function unlock(int ab) external returns (int){
        if(ab > 0) {
            return ab;
        }
        counter++;
        return counter;
    }
}
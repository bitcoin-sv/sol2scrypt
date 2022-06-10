// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract Loop {
    uint constant __LoopCount__0 = 1;
    uint constant __LoopCount__1 = 1;
    function loop() public {
        // for loop
        for (uint i = 0; i < 10; i++) {
            if (i == 3) {
                // Skip to next iteration with continue
                continue;
            }
            if (i == 5) {
                // Exit loop with break
                break;
            }
        }

        // while loop
        uint j;
        while (j < 10) {
            j++;
        }
    }
}

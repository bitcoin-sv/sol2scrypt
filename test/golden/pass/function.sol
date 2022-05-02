// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

library L {
  function f1(uint a, uint b) internal pure returns (uint) {
    return f2(a, b);
  }

  function f2(uint a, uint b) internal pure returns (uint) {
    return a + b;
  }
}

contract Test {
  uint p;

  function f3(uint a) external view {
    require(f4(a) > p);
  }

  function f4(uint a) private pure returns (uint) {
    return L.f1(a, a);
  }
}
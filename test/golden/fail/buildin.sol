// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract C {

    address private owner;

    constructor() {
        owner =  msg.sender;
    }

    function close() external { 

        bytes32 sha = keccak256(abi.encodePacked("aaa"));

        assert(msg.sender == owner);
        selfdestruct(payable(owner)); 
    }

    function recoverSignerFromSignature(uint8 v, bytes32 r, bytes32 s, bytes32 hash) external {
        address signer = ecrecover(hash, v, r, s);
        require(signer != address(0), "ECDSA: invalid signature");
    }

    function callAddMod() public pure returns(uint){
      return addmod(4, 5, 3);
    }
    
    function callMulMod() public pure returns(uint){
        return mulmod(4, 5, 3);
    }
}
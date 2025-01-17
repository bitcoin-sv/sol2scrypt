pragma solidity ^0.8.10;

contract WithdrawalContract {
  address public richest;
  uint public mostSent;

  mapping (address => uint) pendingWithdrawals;

  /// The amount of Ether sent was not higher than
  /// the currently highest amount.
  // error NotEnoughEther();

  constructor() payable {
      richest = msg.sender;
      mostSent = msg.value;
  }

  function becomeRichest() external payable {
      if (msg.value <= mostSent) revert NotEnoughEther();
      pendingWithdrawals[richest] += msg.value;
      richest = msg.sender;
      mostSent = msg.value;
  }

  function withdraw() external {
      uint amount = pendingWithdrawals[msg.sender];
      // Remember to zero the pending refund before
      // sending to prevent re-entrancy attacks
      pendingWithdrawals[msg.sender] = 0;
      // payable(msg.sender).transfer(amount);
  }
}
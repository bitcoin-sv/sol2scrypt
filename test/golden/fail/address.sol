contract C {

    uint immutable maxBalance;

    address private owner;

    constructor(address _reference) {
        owner =  _reference;
        maxBalance = _reference.balance;
    }


    function isBalanceTooHigh(address _other) public view returns (bool) {
        return _other.balance > maxBalance;
    }
    
    function transfer() external payable {
        payable(owner).transfer(msg.value);
    }


    function send(address payable _to) external payable {
        bool sent = _to.send(msg.value);
        require(sent, "Failed to send Ether");
    }

    function call(address payable _to) external payable {
        //(bool sent, bytes memory data) = _to.call{value: msg.value}("");
        //require(sent, "Failed to send Ether");
        _to.call("data");
    }
}
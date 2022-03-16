// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract ERC721 {
    string private constant tokenName = "My ERC721 Token";
    string private constant tokenSymbol = "MET";
    uint256 private constant totalTokens = 1000000;
    mapping(address => uint256) private balances;
    mapping(uint256 => address) private tokenOwners;
    mapping(uint256 => bool) private tokenExists;
    mapping(address => mapping(address => uint256)) private allowed;
    mapping(address => mapping(uint256 => uint256)) private ownerTokens;

    mapping(uint256 => string) tokenLinks;

    function name() external returns (string memory) {
        return tokenName;
    }

    function symbol() external returns (string memory) {
        return tokenSymbol;
    }

    function totalSupply() external returns (uint256) {
        return totalTokens;
    }

    function balanceOf(address _owner) external returns (uint256) {
        return balances[_owner];
    }


    function approve(address _to, uint256 _tokenId) external {
        require(tokenExists[_tokenId]);
        require(msg.sender == tokenOwners[_tokenId]);
        require(msg.sender != _to);
        allowed[msg.sender][_to] = _tokenId;
        emit Approval(msg.sender, _to, _tokenId);
    }

    function takeOwnership(uint256 _tokenId) external {
        require(tokenExists[_tokenId]);
        address oldOwner = tokenOwners[_tokenId];
        address newOwner = msg.sender;
        require(newOwner != oldOwner);
        require(allowed[oldOwner][newOwner] == _tokenId);
        balances[oldOwner] -= 1;
        tokenOwners[_tokenId] = newOwner;
        balances[oldOwner] += 1;
        emit Transfer(oldOwner, newOwner, _tokenId);
    }

    function transfer(address _to, uint256 _tokenId) external {
        address currentOwner = msg.sender;
        address newOwner = _to;
        require(tokenExists[_tokenId]);
        require(currentOwner == tokenOwners[_tokenId]);
        require(currentOwner != newOwner);
        require(newOwner != address(0));

        for (uint256 i = 0; ownerTokens[currentOwner][i] != _tokenId; i++) {
            ownerTokens[currentOwner][i] = 0;
        }

        balances[currentOwner] -= 1;
        tokenOwners[_tokenId] = newOwner;
        balances[newOwner] += 1;
        emit Transfer(currentOwner, newOwner, _tokenId);
    }

    function tokenOfOwnerByIndex(address _owner, uint256 _index)
        external
        returns (uint256 tokenId)
    {
        return ownerTokens[_owner][_index];
    }

    function tokenMetadata(uint256 _tokenId)
        external
        returns (string memory infoUrl)
    {
        return tokenLinks[_tokenId];
    }

    event Transfer(
        address indexed _from,
        address indexed _to,
        uint256 _tokenId
    );
    event Approval(
        address indexed _owner,
        address indexed _approved,
        uint256 _tokenId
    );
}

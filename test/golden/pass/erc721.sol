// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract ERC721 {
    //using Address for address;

    event Transfer(address indexed from, address indexed to, uint indexed tokenId);
    event Approval(
        address indexed owner,
        address indexed approved,
        uint indexed tokenId
    );
    event ApprovalForAll(
        address indexed owner,
        address indexed operator,
        bool approved
    );

    // Mapping from token ID to owner address
    mapping(uint => address) private _owners;

    // Mapping owner address to token count
    mapping(address => uint) private _balances;

    // Mapping from token ID to approved address
    mapping(uint => address) private _tokenApprovals;

    // Mapping from owner to operator approvals
    mapping(address => mapping(address => bool)) private _operatorApprovals;

    function balanceOf(address owner) external view returns (uint) {
        require(owner != address(0), "owner = zero address");
        return _balances[owner];
    }

    function ownerOf(uint tokenId) external view  returns (address owner) {
        owner = _owners[tokenId];
        require(owner != address(0), "token doesn't exist");
    }

    function isApprovedForAll(address owner, address operator)
        external
        view
        
        returns (bool)
    {
        return _operatorApprovals[owner][operator];
    }

    function setApprovalForAll(address operator, bool approved) external  {
        _operatorApprovals[msg.sender][operator] = approved;
        emit ApprovalForAll(msg.sender, operator, approved);
    }

    function getApproved(uint tokenId) external view  returns (address) {
        require(_owners[tokenId] != address(0), "token doesn't exist");
        return _tokenApprovals[tokenId];
    }

    // function _approve(
    //     address owner,
    //     address to,
    //     uint tokenId
    // ) private {
    //     _tokenApprovals[tokenId] = to;
    //     emit Approval(owner, to, tokenId);
    // }

    function approve(address to, uint tokenId) external  {
        address owner = _owners[tokenId];
        require(
            msg.sender == owner || _operatorApprovals[owner][msg.sender],
            "not owner nor approved for all"
        );
        _tokenApprovals[tokenId] = to;
        emit Approval(owner, to, tokenId);
    }

    // function _isApprovedOrOwner(
    //     address owner,
    //     address spender,
    //     uint tokenId
    // ) private view returns (bool) {
    //     return (spender == owner ||
    //         _tokenApprovals[tokenId] == spender ||
    //         _operatorApprovals[owner][spender]);
    // }

    // function _transfer(
    //     address owner,
    //     address from,
    //     address to,
    //     uint tokenId
    // ) private {
    //     require(from == owner, "not owner");
    //     require(to != address(0), "transfer to the zero address");

    //     _approve(owner, address(0), tokenId);

    //     _balances[from] -= 1;
    //     _balances[to] += 1;
    //     _owners[tokenId] = to;

    //     emit Transfer(from, to, tokenId);
    // }

    function transferFrom(
        address from,
        address to,
        uint tokenId
    ) external {
        address owner = _owners[tokenId];
        require(owner != address(0), "token doesn't exist");

        require(msg.sender == owner ||
            _tokenApprovals[tokenId] == msg.sender ||
            _operatorApprovals[owner][msg.sender], "not owner nor approved");

        // do _transfer

        require(from == owner, "not owner");
        require(to != address(0), "transfer to the zero address");

        _tokenApprovals[tokenId] = address(0);
        emit Approval(owner, address(0), tokenId);

        _balances[from] -= 1;
        _balances[to] += 1;
        _owners[tokenId] = to;

        emit Transfer(from, to, tokenId);

    }

    // function _safeTransfer(
    //     address owner,
    //     address from,
    //     address to,
    //     uint tokenId,
    //     bytes memory _data
    // ) private {
    //     _transfer(owner, from, to, tokenId);
    //     //require(_checkOnERC721Received(from, to, tokenId, _data), "not ERC721Receiver");
    // }

    function safeTransferFrom(
        address from,
        address to,
        uint tokenId,
        bytes memory _data
    ) external {
        address owner = _owners[tokenId];
        require(owner != address(0), "token doesn't exist");
        require((msg.sender == owner ||
            _tokenApprovals[tokenId] == msg.sender ||
            _operatorApprovals[owner][msg.sender]), "not owner nor approved");
        // do _safeTransfer
        require(from == owner, "not owner");
        require(to != address(0), "transfer to the zero address");

        _tokenApprovals[tokenId] = address(0);
        emit Approval(owner, address(0), tokenId);

        _balances[from] -= 1;
        _balances[to] += 1;
        _owners[tokenId] = to;

        emit Transfer(from, to, tokenId);
    }

    // // Todo: https://github.com/sCrypt-Inc/sol2scrypt/issues/155
    // // function safeTransferFrom(
    // //     address from,
    // //     address to,
    // //     uint tokenId
    // // ) external {
    // //     safeTransferFrom(from, to, tokenId, "");
    // // }

    function mint(address to, uint tokenId) external {
        require(to != address(0), "mint to zero address");
        require(_owners[tokenId] == address(0), "token already minted");

        _balances[to] += 1;
        _owners[tokenId] = to;

        emit Transfer(address(0), to, tokenId);
    }

    function burn(uint tokenId) external {
        address owner = _owners[tokenId];
        require(owner != address(0), "token doesn't exist");

        _tokenApprovals[tokenId] = address(0);
        emit Approval(owner, address(0), tokenId);

        _balances[owner] -= 1;

        // todo: transpile delete
        // delete _owners[tokenId];

        emit Transfer(owner, address(0), tokenId);
    }
}
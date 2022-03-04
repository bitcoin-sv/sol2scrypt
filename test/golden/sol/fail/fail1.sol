pragma solidity ^0.8.10;

interface ICounter {
    function count() external view returns (uint);

    function increment() external;
}

contract A {
    function foo() public pure virtual returns (string memory) {
        return "A";
    }
}

// Contracts inherit other contracts by using the keyword 'is'.
contract B is A {
    // Override A.foo()
    function foo() public pure virtual override returns (string memory) {
        return "B";
    }
}

// External contract used for try / catch examples
contract Foo {
    address public owner;

    enum Status {
        Pending,
        Shipped,
        Accepted,
        Rejected,
        Canceled
    }

    struct Todo {
        string text;
        bool completed;
    }
    Todo[] public todos;

    constructor(address _owner) {
        require(_owner != address(0), "invalid address");
        owner = _owner;
    }

    modifier onlyOwner() {
        require(msg.sender == owner, "Not owner");
        // Underscore is a special character only used inside
        // a function modifier and it tells Solidity to
        // execute the rest of the code.
        _;
    }

    function myFunc(uint x) public pure returns (string memory) {
        require(x != 0, "require failed");
        return "my func was called";
    }

    function create(string memory _text) public {
        // 3 ways to initialize a struct
        // - calling it like a function
        todos.push(Todo(_text, false));

        // key value mapping
        todos.push(Todo({text: _text, completed: false}));
        
        uint len = todos.length;
    }
}



contract SimpleStorage {
    uint storedData;
    uint public i = 0;
    function set(uint x) external {
        bytes memory a = msg.data;

        uint aa = 1 seconds;
        int min = type(int).min;
        uint c = block.number;

        address d = tx.origin;

        addmod(4, 5, 3);

        mulmod(4, 5, 3);

        (x, x) = (1, 3);

        Foo foo = new Foo(msg.sender);


        while (true) {
            i += 1;
        }

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

        
        storedData = x;
    }

    function get() public view returns (uint) {
        int128 ff = -2 ** 127 ;
        keccak256(abi.encodePacked("aaa"));
        return storedData;
    }

    function testCallFoo(address payable _addr) public payable {
        // You can send ether and specify a custom gas amount
        // (bool success, bytes memory data) = _addr.call{value: msg.value, gas: 5000}(
        //     abi.encodeWithSignature("foo(string,uint256)", "call foo", 123)
        // );
    }
}
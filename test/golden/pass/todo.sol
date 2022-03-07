// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract Todos {
    struct Todo {
        string text;
        bool completed;
    }

    Todo[3] todos;
    function create(string memory _text) external {
        // 3 ways to initialize a struct
        // - calling it like a function
        todos[0] = Todo(_text, false);
        todos[0] = Todo({text: _text, completed: false});

        bool completed = todos[0].completed;

        Todo memory t1 = todos[0];

        t1.completed = false;

        todos[0].completed = false;
    }
    
}
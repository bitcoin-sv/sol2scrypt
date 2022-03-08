// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract Todos {
    struct Todo {
        string text;
        bool completed;
    }

    Todo[3] todos;
    function create(string memory _text) public returns (Todo memory) {
        // 3 ways to initialize a struct
        // 1. calling it like a function
        todos[0] = Todo(_text, false);
        // 2. key value mapping
        todos[0] = Todo({text: _text, completed: false});

        bool completed = todos[0].completed;

        Todo memory t1 = todos[0];

        t1.completed = false;

        todos[0].completed = false;
        // 3. initialize an empty struct and then update it
        // Todo memory todo;
        // todo.text = _text;

        //return Todo(_text, true);
    }


    function return1(string memory _text) public returns (Todo memory) {

        if(true) {
            return Todo(_text, false);
        }
        return todos[0];
    }

    function return2(string memory _text) public returns (Todo[3] memory) {

        if(true) {
            return todos;
        }

        return todos;
    }
    
}
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;


struct ST {
    string text;
    bool completed;
    uint8[3] cl;
}


struct STW {
    ST[1] st;
    string text;
}


contract Todos {
    struct Todo {
        string text;
        bool completed;
    }

    struct TodoW {
        string text;
        bool completed;
        STW stw;
    }

    Todo[3] todos;
    STW stw;

    TodoW tw;
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

    function return3(string memory _text) public returns (STW memory) {

        if(true) {
            return stw;
        }
        return STW([ST(_text, false, [1,3,3])], _text);
    }

    function return4(string memory _text) public returns (TodoW memory) {

        if(true) {
            return tw;
        }
        return tw;
    }


    function param(TodoW memory tw) public  {

        TodoW memory a = tw;

    }
    
}
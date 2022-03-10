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



    mapping(address => Todo) public mapTodos;

    //test declare
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

    function update(string memory _text, address owner) public  {
        mapTodos[owner] = Todo(_text, true);
    }


    function testReturnStructArray(string memory _text) public view returns (Todo[3] memory) {

        if(true) {
            return todos;
        }

        return todos;
    }


    function testReturnNestStruct(string memory _text) public view returns (TodoW memory) {

        if(true) {
            return tw;
        }
        return tw;
    }


    function testStructAsParam(TodoW memory tw) public pure {

        TodoW memory a = tw;

    }
    
}
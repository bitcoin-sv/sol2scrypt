# sol2scrypt: A Solidity to sCrypt Transpiler
[![Build Status](https://app.travis-ci.com/sCrypt-Inc/sol2scrypt.svg?token=v6N8VybyjmC1GLSWZv92&branch=master)](https://travis-ci.com/sCrypt-Inc/sol2scrypt)

# Install
Easies way to install is to download from [Releases](https://github.com/sCrypt-Inc/sol2scrypt/releases).

# Usage
```sh
> sol2scrypt transpile --help                
Usage: sol2scrypt transpile [-o|--output-dir OUTPUTDIR] [FILE] [-F|--force]
                            
  Transpile Solidity FILE to sCrypt

Available options:
  -o,--output-dir OUTPUTDIR
                           Output directory if given, output to stdout if none given
  FILE                     Source file path. Take source file content from stdin
                           if none given
  -F,--force               Whether to output scrypt code despite errors
  -h,--help                Show this help text


> sol2scrypt transpile examples/Coin/Coin.sol -o ~/tmp
transpile result written to `/Users/foo/tmp/Coin.scrypt`
```

  # Documentation
  A [cheatsheet](https://github.com/sCrypt-Inc/sol2scrypt/tree/master/docs) for currently un/supported features. Examples can be found at [tests](https://github.com/sCrypt-Inc/sol2scrypt/tree/master/test/golden).
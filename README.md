# sol2scrypt: A Solidity to sCrypt Transpiler

# Install
Easies way to install is to download from [Releases](https://github.com/sCrypt-Inc/sol2scrypt/releases).

# Usage
```sh
> sol2scrypt transpile --help                
Usage: sol2scrypt transpile [-o|--output-dir OUTPUTDIR] [FILE] [-L|--log] 
                            [-F|--force]
  Transpile Solidity FILE to sCrypt

Available options:
  -o,--output-dir OUTPUTDIR
                           Output directory if given, default to current
                           directory
  FILE                     Source file path. Take source file content from stdin
                           if none given
  -L,--log                 Whether to output errors in json format
  -F,--force               Whether to output result file despite errors
  -h,--help                Show this help text


> sol2scrypt transpile examples/Coin/Coin.sol -L -o ~/tmp
transpile result written to `/Users/foo/tmp/Coin.scrypt`
```

  # Documentation
  A [cheatsheet](https://github.com/sCrypt-Inc/sol2scrypt/tree/master/docs) for currently un/supported features. Examples can be found at [tests](https://github.com/sCrypt-Inc/sol2scrypt/tree/master/test/golden).
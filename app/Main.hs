{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import IR
import Scrypt as Scr
import Solidity as Sol
import Transpiler
import Utils
import Text.RawString.QQ

main :: IO ()
main = do

  -- let solidityCode1 = "uint256"
  -- tr1 :: TranspileResult (TypeName' SourceRange) IType' (Maybe Type) <- transpile solidityCode1
  -- putStrLn $ "Transpile `" ++ solidityCode1 ++ "` to `" ++ scryptCode tr1 ++ "`"

  -- let solidityCode2 = "0x123a"
  -- tr2 :: TranspileResult Expression IExpression' (Maybe (Expr Ann)) <- transpile solidityCode2
  -- putStrLn $ "Transpile `" ++ solidityCode2 ++ "` to `" ++ scryptCode tr2 ++ "`"


  -- let solidityCode = "function set(uint x) external { x; }"
  -- f :: ContractPart <- parseIO solidityCode
  -- print f

  -- let solidityCode3 = "function set(uint x) external { 1; }"
  -- tr3 :: TranspileResult ContractPart IFunction' (Maybe (Scr.Function Ann)) <- transpile solidityCode3
  -- putStrLn $ "Transpile `" ++ solidityCode3 ++ "` to `" ++ scryptCode tr3 ++ "`"



  -- let solidityCode4 = "uint256[2] storedData;"
  -- e :: TypeName' SourceRange <- parseIO solidityCode4
  -- print e


  let solidityCode5 = [r|
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

import "./myLovelyLovelyLib.sol";
import "ccc.sol";

contract SimpleStorage {
    uint storedData;

    constructor() {}
}

contract flipper {
    bool private value;

    /// Constructor that initializes the `bool` value to the given `init_value`.
    constructor(bool initvalue) {
        value = initvalue;
    }

    /// A message that can be called on instantiated contracts.
    /// This one flips the value of the stored `bool` from `true`
    /// to `false` and vice versa.
    function flip() public {
        require(value, "an error message");
        value = !value;
    }

    /// Simply returns the current value of our `bool`.
    function get() public view returns (bool) {
        return value;
    }
}
|]

  e :: SolidityCode' SourceRange <- parseIO solidityCode5
  print e


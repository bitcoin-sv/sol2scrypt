{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Transformations.Sol2IR.Program where

import IR.Spec as IR
import IR.Transformer
import Solidity.Spec
import Test.Tasty
import Test.Tasty.Hspec
import Transformations.Helper
import Text.RawString.QQ

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable SolidityCode Program'" $ do
  let itProgram sol e = it "should transfrom Solidity `SolidityCode` to IR Program correctly" $ do
        r1 <- sol2Ir sol2Program sol
        r1 `shouldBe` Just e

  describe "#Empty Program" $ do
    itProgram  ""  ( IR.Program [] [] [])

  describe "#Simple Program" $ do
    itProgram  [r|
contract SimpleStorage {
  uint storedData;

  constructor() {}
}   
|]  (Program {programImports = [], programContracts = [Contract {contractName = (IR.Identifier "SimpleStorage"), contractBody = [IR.StateVariableDeclaration (StateVariable {stateVarName = (IR.Identifier "storedData"), stateVarType = ElementaryType Int, stateVisibility = Default, stateInitialValue = Nothing, stateIsConstant = False}),ConstructorDefinition (Constructor {ctorParams = ParamList [], ctorBody = IR.Block []})]}], programLibraries = []})


  describe "#Program with pragma and import" $ do
    itProgram  [r|
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

import "./myLovelyLovelyLib.sol";

contract SimpleStorage {
  uint storedData;

  constructor() {}
}   
|]  (Program {programImports = [IR.ImportDirective "./myLovelyLovelyLib.scrypt"], programContracts = [IR.Contract {contractName = (IR.Identifier "SimpleStorage"), contractBody = [IR.StateVariableDeclaration (IR.StateVariable {stateVarName = (IR.Identifier "storedData"), stateVarType = ElementaryType Int, stateVisibility = Default, stateInitialValue = Nothing, stateIsConstant = False}),ConstructorDefinition (Constructor {ctorParams = ParamList [], ctorBody = IR.Block []})]}], programLibraries = []})


  describe "#Program with pragma and multi import" $ do
    itProgram  [r|
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

import "./myLovelyLovelyLib.sol";
import "./aa.sol";

|]  (Program {programImports = [IR.ImportDirective "./myLovelyLovelyLib.scrypt", IR.ImportDirective "./aa.scrypt"], programContracts = [], programLibraries = []})
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformations.Sol2IR.Contract where

import IR.Spec as IR
import IR.Transformer
import Solidity.Spec as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transformations.Helper
import Text.RawString.QQ

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable Contractpart IContractBodyElement'" $ do
  let itContractPart solidityCode target = it "should transfrom Solidity `ContractPartStateVariableDeclaration` to IR Function correctly" $ do
        ir :: IContractBodyElement' <- sol2Ir sol2ContractPart solidityCode
        ir `shouldBe` target

  let itContract solidityCode target = it "should transfrom Solidity `Contract` to IR Function correctly" $ do
        ir :: IContract' <- sol2Ir sol2Contract solidityCode
        ir `shouldBe` target


  describe "state property" $ do
    itContractPart "uint a;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Default Nothing False))
    itContractPart "uint private a;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Private Nothing False))
    itContractPart "uint public a;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Public Nothing False))
    itContractPart "int public a;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Public Nothing False))
    itContractPart "int public a = 1;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Public
      (Just (LiteralExpr (IntLiteral {isHex = False, intVal = 1}))) False))
    itContractPart "bool a;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Bool) Default Nothing False))
    itContractPart "bool a = true;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Bool) Default
      (Just (LiteralExpr (BoolLiteral True))) False))

  describe "constant property" $ do
    itContractPart "uint constant a = 8;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Default (Just (LiteralExpr (IntLiteral {isHex = False, intVal = 8}))) True))
    itContractPart "uint constant a = 8 * 3 + (1 % 1);" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Default (Just (BinaryExpr {binaryOp = Add, lExpr = BinaryExpr {binaryOp = Mul, lExpr = LiteralExpr (IntLiteral {isHex = False, intVal = 8}), rExpr = LiteralExpr (IntLiteral {isHex = False, intVal = 3})}, rExpr = ParensExpr {enclosedExpr = BinaryExpr {binaryOp = Mod, lExpr = LiteralExpr (IntLiteral {isHex = False, intVal = 1}), rExpr = LiteralExpr (IntLiteral {isHex = False, intVal = 1})}}})) True))

  describe "Event" $ do
    itContractPart "event Log(address indexed sender, string message);" Nothing
    itContractPart "event Log();" Nothing

  describe "constructor" $ do
    itContractPart "constructor(bool initvalue) {  value = initvalue;  }" $ Just (ConstructorDefinition (Constructor {ctorParams = ParamList [Param {paramType = ElementaryType Bool, paramName = IR.Identifier "initvalue"}], ctorBody = IR.Block [AssignStmt [IR.IdentifierExpr (IR.Identifier "value")] [IdentifierExpr (IR.Identifier "initvalue")]]}))
    itContractPart "constructor() { }" $ Just (IR.ConstructorDefinition (IR.Constructor {IR.ctorParams = ParamList [], IR.ctorBody = IR.Block []}))
    itContractPart "constructor(int a, bool b) { c = a++; d=b; }" $ Just (ConstructorDefinition (Constructor {ctorParams = ParamList [Param {paramType = ElementaryType Int, paramName = IR.Identifier "a"},Param {paramType = ElementaryType Bool, paramName = IR.Identifier "b"}], ctorBody = IR.Block [AssignStmt [IR.IdentifierExpr (IR.Identifier "c")] [UnaryExpr {unaryOp = PostIncrement, uExpr = IdentifierExpr (IR.Identifier "a")}],AssignStmt [IR.IdentifierExpr (IR.Identifier "d")] [IdentifierExpr (IR.Identifier "b")]]}))
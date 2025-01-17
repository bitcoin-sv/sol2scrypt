
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Transformations.Sol2IR.Contract where

import IR.Spec as IR
import Test.Tasty
import Test.Tasty.Hspec
import Transformations.Helper

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable Contractpart IContractBodyElement'" $ do
  let itContractPart solidityCode target = it "should transfrom Solidity `ContractPartStateVariableDeclaration` to IR Function correctly" $ do
        ir :: IContractBodyElement' <- sol2Ir sol2ContractPart solidityCode
        ir `shouldBe` target


  describe "state property" $ do
    itContractPart "uint a;" $ Just (IR.PropertyDefinition (IR.Property (IR.Identifier "a") (ElementaryType Int) Default Nothing (IsConst False) (IsStatic False) (IsState True)))
    itContractPart "uint private a;" $ Just (IR.PropertyDefinition (IR.Property (IR.Identifier "a") (ElementaryType Int) Private Nothing (IsConst False) (IsStatic False) (IsState True)))
    itContractPart "uint public a;" $ Just (IR.PropertyDefinition (IR.Property (IR.Identifier "a") (ElementaryType Int) Public Nothing (IsConst False) (IsStatic False) (IsState True)))
    itContractPart "int public a;" $ Just (IR.PropertyDefinition (IR.Property (IR.Identifier "a") (ElementaryType Int) Public Nothing (IsConst False) (IsStatic False) (IsState True)))
    itContractPart "bool a;" $ Just (IR.PropertyDefinition (IR.Property (IR.Identifier "a") (ElementaryType Bool) Default Nothing (IsConst False) (IsStatic False) (IsState True)))

  describe "constant property" $ do
    itContractPart "uint constant a = 8;" $ Just (IR.PropertyDefinition (IR.Property (IR.Identifier "a") (ElementaryType Int) Default (Just (LiteralExpr (IntLiteral {isHex = False, intVal = 8}))) (IsConst True) (IsStatic True) (IsState False)))
    itContractPart "uint constant a = 8 * 3 + (1 % 1);" $ Just (IR.PropertyDefinition (IR.Property (IR.Identifier "a") (ElementaryType Int) Default (Just (BinaryExpr {binaryOp = Add, lExpr = BinaryExpr {binaryOp = Mul, lExpr = LiteralExpr (IntLiteral {isHex = False, intVal = 8}), rExpr = LiteralExpr (IntLiteral {isHex = False, intVal = 3})}, rExpr = ParensExpr {enclosedExpr = BinaryExpr {binaryOp = Mod, lExpr = LiteralExpr (IntLiteral {isHex = False, intVal = 1}), rExpr = LiteralExpr (IntLiteral {isHex = False, intVal = 1})}}})) (IsConst True) (IsStatic True) (IsState False)))

  describe "Event" $ do
    itContractPart "event Log(address indexed sender, string message);" Nothing
    itContractPart "event Log();" Nothing

  describe "constructor" $ do
    itContractPart "constructor(bool initvalue) {  value = initvalue;  }" $ Just (ConstructorDefinition (Constructor {ctorParams = ParamList [Param {paramType = ElementaryType Bool, paramName = IR.Identifier "initvalue"}], ctorBody = IR.Block [AssignStmt [IR.IdentifierExpr (IR.Identifier "value")] [IdentifierExpr (IR.Identifier "initvalue")]]}))
    itContractPart "constructor() { }" $ Just (IR.ConstructorDefinition (IR.Constructor {IR.ctorParams = ParamList [], IR.ctorBody = IR.Block []}))
    itContractPart "constructor(int a, bool b) { c = a++; d=b; }" $ Just (ConstructorDefinition (Constructor {ctorParams = ParamList [Param {paramType = ElementaryType Int, paramName = IR.Identifier "a"},Param {paramType = ElementaryType Bool, paramName = IR.Identifier "b"}], ctorBody = IR.Block [AssignStmt [IR.IdentifierExpr (IR.Identifier "c")] [UnaryExpr {unaryOp = PostIncrement, uExpr = IdentifierExpr (IR.Identifier "a")}],AssignStmt [IR.IdentifierExpr (IR.Identifier "d")] [IdentifierExpr (IR.Identifier "b")]]}))

  describe "immutable property" $ do
    itContractPart "uint immutable a;" $ Just (IR.PropertyDefinition (IR.Property (IR.Identifier "a") (ElementaryType Int) Default Nothing (IsConst True) (IsStatic False) (IsState False)))
    itContractPart "address immutable owner;" $ Just (IR.PropertyDefinition (IR.Property (IR.Identifier "owner") (ElementaryType Address) Default Nothing (IsConst True) (IsStatic False) (IsState False)))
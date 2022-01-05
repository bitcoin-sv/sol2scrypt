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


  describe "when the function is `public pure`" $ do
    itContractPart "uint a;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Default Nothing))
    itContractPart "uint private a;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Private Nothing))
    itContractPart "uint public a;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Public Nothing))
    itContractPart "int public a;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Public Nothing))
    itContractPart "int public a = 1;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Int) Public 
      (Just (LiteralExpr (IntLiteral {isHex = False, intVal = 1})))))
    itContractPart "bool a;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Bool) Default Nothing))
    itContractPart "bool a = true;" $ Just (IR.StateVariableDeclaration (IR.StateVariable (IR.Identifier "a") (ElementaryType Bool) Default 
      (Just (LiteralExpr (BoolLiteral True)))))

  describe "Event" $ do
    itContractPart "event Log(address indexed sender, string message);" Nothing
    itContractPart "event Log();" Nothing

  describe "constructor" $ do
    itContractPart "constructor(bool initvalue) {  value = initvalue;  }" $ Just (ConstructorDefinition (Constructor {ctorParams = ParamList [Param {paramType = ElementaryType Bool, paramName = (IR.Identifier "initvalue")}], ctorBody = IR.Block [AssignStmt [Just (IR.Identifier "value")] [IdentifierExpr (IR.Identifier "initvalue")]]}))
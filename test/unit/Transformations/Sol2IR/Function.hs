{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Transformations.Sol2IR.Function where

import IR.Spec as IR
import Test.Tasty
import Test.Tasty.Hspec
import Transformations.Helper

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable Contractpart IFunction'" $ do
  let itTransformFunc solidityCode target = it "should transfrom Solidity `ContractPartFunctionDefinition` to IR Function correctly" $ do
        ir :: IFunction' <- sol2Ir sol2ContractPart solidityCode
        ir `shouldBe` target

  let funcWithoutRet vis mut = "function add(uint x, uint y) " ++ vis ++ " " ++ mut ++ " {}"
      funcWithOnlyTypeRet vis mut = "function add(uint x, uint y) " ++ vis ++ " " ++ mut ++ " returns (uint) { return x + y; }"
      funcWithNamedRet vis mut = "function add(uint x, uint y) " ++ vis ++ " " ++ mut ++ " returns (uint z)  { z = x + y; }"
      funcWithMultiRet vis mut = "function add(uint x, uint y) " ++ vis ++ " " ++ mut ++ " returns (uint, uint) {}"

      -- IR non-public function without `preimage` param
  let expFuncWithoutRet_G1 vis = Just (Function {funcName = IR.Identifier "add", funcParams = ParamList [Param {paramType = ElementaryType Int, paramName = IR.Identifier "x"},Param {paramType = ElementaryType Int, paramName = IR.Identifier "y"}], funcBody = IR.Block [ReturnStmt {retExpr = LiteralExpr (BoolLiteral True)}], funcReturn = ElementaryType Bool, funcVisibility = vis, IR.funcStatic = False})
      expFuncWithOnlyTypeRet_G1 vis =  Just (Function {funcName = IR.Identifier "add", funcParams = ParamList [Param {paramType = ElementaryType Int, paramName = IR.Identifier "x"},Param {paramType = ElementaryType Int, paramName = IR.Identifier "y"}], funcBody = IR.Block [ReturnStmt {retExpr = BinaryExpr {binaryOp = Add, lExpr = IdentifierExpr (IR.Identifier "x"), rExpr = IdentifierExpr (IR.Identifier "y")}}], funcReturn = ElementaryType Int, funcVisibility = vis, IR.funcStatic = False})
      expFuncWithNamedRet_G1 vis = Just (Function {funcName = IR.Identifier  "add", funcParams = ParamList [Param {paramType = ElementaryType Int, paramName = IR.Identifier  "x"},Param {paramType = ElementaryType Int, paramName = IR.Identifier  "y"}], funcBody = IR.Block [DeclareStmt [Just (Param {paramType = ElementaryType Int, paramName = IR.Identifier  "z"})] [LiteralExpr (IntLiteral {isHex = False, intVal = 0})],AssignStmt [IR.IdentifierExpr (IR.Identifier  "z")] [BinaryExpr {binaryOp = Add, lExpr = IdentifierExpr (IR.Identifier  "x"), rExpr = IdentifierExpr (IR.Identifier  "y")}],ReturnStmt {retExpr = IdentifierExpr (IR.Identifier  "z")}], funcReturn = ElementaryType Int, funcVisibility = vis, IR.funcStatic = False})

      -- IR public function with `preimage` param
      expFuncWithoutRet_G3 vis = Just (Function {funcName = IR.Identifier "add", funcParams = ParamList [Param {paramType = ElementaryType Int, paramName = IR.Identifier "x"},Param {paramType = ElementaryType Int, paramName = IR.Identifier "y"},Param {paramType = BuiltinType "SigHashPreimage", paramName = ReservedId "txPreimage"}], funcBody = IR.Block [RequireStmt {verifyExpr = FunctionCallExpr {funcExpr = MemberAccessExpr {instanceExpr = IdentifierExpr (IR.Identifier "this"), member = IR.Identifier "propagateState"}, funcParamExprs = [IdentifierExpr (ReservedId "txPreimage")]}}], funcReturn = ElementaryType Bool, funcVisibility = vis, IR.funcStatic = False})
      expFuncWithOnlyTypeRet_G3 vis = Just (Function {funcName = IR.Identifier "add", funcParams = ParamList [Param {paramType = ElementaryType Int, paramName = IR.Identifier "x"},Param {paramType = ElementaryType Int, paramName = IR.Identifier "y"},Param {paramType = BuiltinType "SigHashPreimage", paramName = ReservedId "txPreimage"},Param {paramType = ElementaryType Int, paramName = IR.Identifier "retVal"}], funcBody = IR.Block [RequireStmt {verifyExpr = BinaryExpr {binaryOp = IR.Equal, lExpr = BinaryExpr {binaryOp = Add, lExpr = IdentifierExpr (IR.Identifier "x"), rExpr = IdentifierExpr (IR.Identifier "y")}, rExpr = IdentifierExpr (IR.Identifier "retVal")}},RequireStmt {verifyExpr = FunctionCallExpr {funcExpr = MemberAccessExpr {instanceExpr = IdentifierExpr (IR.Identifier "this"), member = IR.Identifier "propagateState"}, funcParamExprs = [IdentifierExpr (ReservedId "txPreimage")]}}], funcReturn = ElementaryType Bool, funcVisibility = vis, IR.funcStatic = False})
      expFuncWithNamedRet_G3 vis = Just (Function {funcName = IR.Identifier "add", funcParams = ParamList [Param {paramType = ElementaryType Int, paramName = IR.Identifier "x"},Param {paramType = ElementaryType Int, paramName = IR.Identifier "y"},Param {paramType = BuiltinType "SigHashPreimage", paramName = ReservedId "txPreimage"},Param {paramType = ElementaryType Int, paramName = IR.Identifier "_z"}], funcBody = IR.Block [DeclareStmt [Just (Param {paramType = ElementaryType Int, paramName = IR.Identifier "z"})] [LiteralExpr (IntLiteral {isHex = False, intVal = 0})],AssignStmt [IdentifierExpr (IR.Identifier "z")] [BinaryExpr {binaryOp = Add, lExpr = IdentifierExpr (IR.Identifier "x"), rExpr = IdentifierExpr (IR.Identifier "y")}],RequireStmt {verifyExpr = BinaryExpr {binaryOp = IR.Equal, lExpr = IdentifierExpr (IR.Identifier "z"), rExpr = IdentifierExpr (IR.Identifier "_z")}},RequireStmt {verifyExpr = FunctionCallExpr {funcExpr = MemberAccessExpr {instanceExpr = IdentifierExpr (IR.Identifier "this"), member = IR.Identifier "propagateState"}, funcParamExprs = [IdentifierExpr (ReservedId "txPreimage")]}}], funcReturn = ElementaryType Bool, funcVisibility = vis, IR.funcStatic = False})

      -- IR public function with `preimage` param
      expFuncWithoutRet_G4 vis = Just (Function {funcName = IR.Identifier "add", funcParams = ParamList [Param {paramType = ElementaryType Int, paramName = IR.Identifier "x"},Param {paramType = ElementaryType Int, paramName = IR.Identifier "y"},Param {paramType = BuiltinType "SigHashPreimage", paramName = ReservedId "txPreimage"}], funcBody = IR.Block [RequireStmt {verifyExpr = FunctionCallExpr {funcExpr = MemberAccessExpr {instanceExpr = IdentifierExpr (IR.Identifier "this"), member = IR.Identifier "propagateState"}, funcParamExprs = [IdentifierExpr (IR.ReservedId "txPreimage")]}}], funcReturn = ElementaryType Bool, funcVisibility = vis, IR.funcStatic = False})
      expFuncWithOnlyTypeRet_G4 vis = Just (Function {funcName = IR.Identifier "add", funcParams = ParamList [Param {paramType = ElementaryType Int, paramName = IR.Identifier "x"},Param {paramType = ElementaryType Int, paramName = IR.Identifier "y"},Param {paramType = BuiltinType "SigHashPreimage", paramName = ReservedId "txPreimage"},Param {paramType = ElementaryType Int, paramName = IR.Identifier "retVal"}], funcBody = IR.Block [RequireStmt {verifyExpr = BinaryExpr {binaryOp = IR.Equal, lExpr = BinaryExpr {binaryOp = Add, lExpr = IdentifierExpr (IR.Identifier "x"), rExpr = IdentifierExpr (IR.Identifier "y")}, rExpr = IdentifierExpr (IR.Identifier "retVal")}},RequireStmt {verifyExpr = FunctionCallExpr {funcExpr = MemberAccessExpr {instanceExpr = IdentifierExpr (IR.Identifier "this"), member = IR.Identifier "propagateState"}, funcParamExprs = [IdentifierExpr (ReservedId "txPreimage")]}}], funcReturn = ElementaryType Bool, funcVisibility = vis, IR.funcStatic = False})
      expFuncWithNamedRet_G4 vis = Just (Function {funcName = IR.Identifier "add", funcParams = ParamList [Param {paramType = ElementaryType Int, paramName = IR.Identifier "x"},Param {paramType = ElementaryType Int, paramName = IR.Identifier "y"},Param {paramType = BuiltinType "SigHashPreimage", paramName = ReservedId "txPreimage"},Param {paramType = ElementaryType Int, paramName = IR.Identifier "_z"}], funcBody = IR.Block [DeclareStmt [Just (Param {paramType = ElementaryType Int, paramName = IR.Identifier "z"})] [LiteralExpr (IntLiteral {isHex = False, intVal = 0})],AssignStmt [IdentifierExpr (IR.Identifier "z")] [BinaryExpr {binaryOp = Add, lExpr = IdentifierExpr (IR.Identifier "x"), rExpr = IdentifierExpr (IR.Identifier "y")}],RequireStmt {verifyExpr = BinaryExpr {binaryOp = IR.Equal, lExpr = IdentifierExpr (IR.Identifier "z"), rExpr = IdentifierExpr (IR.Identifier "_z")}},RequireStmt {verifyExpr = FunctionCallExpr {funcExpr = MemberAccessExpr {instanceExpr = IdentifierExpr (IR.Identifier "this"), member = IR.Identifier "propagateState"}, funcParamExprs = [IdentifierExpr (ReservedId "txPreimage")]}}], funcReturn = ElementaryType Bool, funcVisibility = vis, IR.funcStatic = False})

  describe "when the function is `public pure`" $ do
    let vis = "public"; mut = "pure"
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G1 Default
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G1 Default
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G1 Default
    itTransformFunc (funcWithMultiRet vis mut) Nothing

  describe "when the function is `public view`" $ do
    let vis = "public"; mut = "view"
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G1 Default
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G1 Default
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G1 Default
    itTransformFunc (funcWithMultiRet vis mut) Nothing

  describe "when the function is `public payable`" $ do
    let vis = "public"; mut = "payable"
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G1 Default
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G1 Default
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G1 Default
    itTransformFunc (funcWithMultiRet vis mut) Nothing

  describe "when the function is `public non-payable`" $ do
    let vis = "public"; mut = ""
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G1 Default
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G1 Default
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G1 Default
    itTransformFunc (funcWithMultiRet vis mut) Nothing

  describe "when the function is `private pure`" $ do
    let vis = "private"; mut = "pure"
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G1 Private
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G1 Private
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G1 Private
    itTransformFunc (funcWithMultiRet vis mut) Nothing

  describe "when the function is `private view`" $ do
    let vis = "private"; mut = "view"
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G1 Private
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G1 Private
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G1 Private
    itTransformFunc (funcWithMultiRet vis mut) Nothing

  describe "when the function is `private payable`" $ do
    let vis = "private"; mut = "payable"
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G1 Private
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G1 Private
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G1 Private
    itTransformFunc (funcWithMultiRet vis mut) Nothing

  describe "when the function is `private non-payable`" $ do
    let vis = "private"; mut = ""
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G1 Private
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G1 Private
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G1 Private
    itTransformFunc (funcWithMultiRet vis mut) Nothing

  describe "when the function is `internal pure`" $ do
    let vis = "internal"; mut = "pure"
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G1 Private
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G1 Private
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G1 Private
    itTransformFunc (funcWithMultiRet vis mut) Nothing

  describe "when the function is `internal view`" $ do
    let vis = "internal"; mut = "view"
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G1 Private
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G1 Private
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G1 Private
    itTransformFunc (funcWithMultiRet vis mut) Nothing

  describe "when the function is `internal payable`" $ do
    let vis = "internal"; mut = "payable"
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G1 Private
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G1 Private
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G1 Private
    itTransformFunc (funcWithMultiRet vis mut) Nothing

  describe "when the function is `internal non-payable`" $ do
    let vis = "internal"; mut = ""
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G1 Private
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G1 Private
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G1 Private
    itTransformFunc (funcWithMultiRet vis mut) Nothing

  describe "when the function is `external pure`" $ do
    let vis = "external"; mut = "pure"
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G3 Public
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G3 Public
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G3 Public
    itTransformFunc (funcWithMultiRet vis mut) Nothing

  describe "when the function is `external view`" $ do
    let vis = "external"; mut = "view"
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G4 Public
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G4 Public
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G4 Public
    itTransformFunc (funcWithMultiRet vis mut) Nothing

  describe "when the function is `external payable`" $ do
    let vis = "external"; mut = "payable"
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G4 Public
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G4 Public
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G4 Public
    itTransformFunc (funcWithMultiRet vis mut) Nothing

  describe "when the function is `external non-payable`" $ do
    let vis = "external"; mut = ""
    itTransformFunc (funcWithoutRet vis mut) $ expFuncWithoutRet_G4 Public
    itTransformFunc (funcWithOnlyTypeRet vis mut) $ expFuncWithOnlyTypeRet_G4 Public
    itTransformFunc (funcWithNamedRet vis mut) $ expFuncWithNamedRet_G4 Public
    itTransformFunc (funcWithMultiRet vis mut) Nothing
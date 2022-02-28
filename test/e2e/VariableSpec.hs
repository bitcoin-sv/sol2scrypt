{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module VariableSpec where

import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transpiler
import Utils
import Control.Exception

transpileProperty :: String -> IO String
transpileProperty sol = do
  tr :: TranspileResult ContractPart IContractBodyElement' (Maybe (Scr.Param Ann)) <- transpile sol
  return $ scryptCode tr


transpileParameter :: String -> IO String
transpileParameter sol = do
  tr :: TranspileResult Parameter IParam' (Maybe (Scr.Param Ann)) <- transpile sol
  return $ scryptCode tr

transpileStatic :: String -> IO String
transpileStatic sol = do
  tr :: TranspileResult ContractPart IContractBodyElement' (Maybe (Scr.Param Ann)) <- transpile sol
  return $ scryptCode tr

spec :: IO TestTree
spec = testSpec "Transpile Variable" $ do

  let itProperty sol scrypt = it ("should transpile Solidity `" ++ sol ++ "` correctly") $ do
        tr :: TranspileResult (ContractPart SourceRange) IContractBodyElement' (Maybe (Scr.Param Ann)) <- transpile sol
        scryptCode tr `shouldBe` scrypt
  let itParameter sol scrypt = it ("should transpile Solidity `" ++ sol ++ "` correctly") $ do
        tr :: TranspileResult (Parameter SourceRange) IParam' (Maybe (Scr.Param Ann)) <- transpile sol
        scryptCode tr `shouldBe` scrypt
  let itStatic sol scrypt = it ("should transpile Solidity `" ++ sol ++ "` correctly") $ do
            tr :: TranspileResult (ContractPart SourceRange) IContractBodyElement' (Maybe (Scr.Static Ann)) <- transpile sol
            scryptCode tr `shouldBe` scrypt

  let itPropertyThrow sol err = it ("should throw when transpiling Solidity `" ++ sol ++ "`") $ do
        transpileProperty sol `shouldThrow` err  

  let itParameterThrow sol err = it ("should throw when transpiling Solidity `" ++ sol ++ "`") $ do
        transpileProperty sol `shouldThrow` err  

  let itStaticThrow sol err = it ("should throw when transpiling Solidity `" ++ sol ++ "`") $ do
        transpileStatic sol `shouldThrow` err  

  describe "#Property" $ do
      itProperty "uint storedData;" "\n@state\nint storedData;"
      itProperty "int storedData;" "\n@state\nint storedData;"
      itProperty "bool a;" "\n@state\nbool a;"
      itProperty "bytes a;" "\n@state\nbytes a;"
      itProperty "bytes private a;" "\n@state\nprivate bytes a;"
      itProperty "bytes public a;" "\n@state\npublic bytes a;"
      itProperty "string public a;" "\n@state\npublic bytes a;"
      itProperty "address public a;" "\n@state\npublic PubKeyHash a;"
      itPropertyThrow "uint aa = 3;" (errorCall "IStateVariable to (Scr.Param Ann) `StateVariable {stateVarName = (Identifier \"aa\"), stateVarType = ElementaryType Int, stateVisibility = Default, stateInitialValue = Just (LiteralExpr (IntLiteral {isHex = False, intVal = 3})), stateIsConstant = False, stateIsImmutable = False}` not implemented in scrypt")
      itPropertyThrow "uint[] aa;" (errorCall "unsupported expression to Integer : `Nothing`")
      itPropertyThrow "address immutable owner = msg.sender;" (errorCall "IStateVariable to (Scr.Param Ann) `StateVariable {stateVarName = (Identifier \"owner\"), stateVarType = ElementaryType Address, stateVisibility = Default, stateInitialValue = Just (IdentifierExpr (ReservedId \"msgSender\")), stateIsConstant = False, stateIsImmutable = True}` not implemented in scrypt")
      itPropertyThrow "address payable a;" (errorCall "unsupported type `TypeNameElementaryTypeName`")
  describe "#Parameter" $ do
    itParameter "int a" "int a"
    itParameter "uint a" "int a"
    itParameter "byte a" "bytes a"
    itParameter "bytes a" "bytes a"
    itParameter "string a" "bytes a"
    itParameter "address a" "PubKeyHash a"
    itParameter "bool a" "bool a"
    itParameterThrow "bytes[1] x"  anyIOException

  describe "#Static" $ do
    itStatic "uint constant x = 1;" "\nstatic const int x = 1;"
    itStatic "uint constant x = 1 + 1 *(1-1);" "\nstatic const int x = 1 + 1 * (1 - 1);"
    itStatic "bool constant x = true;" "\nstatic const bool x = true;"
    itStatic "int constant internal INT_TWO = 2;" "\nstatic const int INT_TWO = 2;"
    itStaticThrow "int constant internal UINT_MIN = int(2 ** 4);" anyException
    itStaticThrow "int constant  UINT_MIN = 1 + 3;" (errorCall "IStateVariable to (Scr.Param Ann) `StateVariable {stateVarName = (Identifier \"UINT_MIN\"), stateVarType = ElementaryType Int, stateVisibility = Default, stateInitialValue = Just (BinaryExpr {binaryOp = Add, lExpr = LiteralExpr (IntLiteral {isHex = False, intVal = 1}), rExpr = LiteralExpr (IntLiteral {isHex = False, intVal = 3})}), stateIsConstant = True, stateIsImmutable = False}` not implemented in scrypt")
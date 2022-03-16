{-# LANGUAGE ScopedTypeVariables #-}

module VariableSpec where

import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transpiler
import Utils
import Helper

transpileProperty :: String -> IO (String, Logs)
transpileProperty sol = do
  tr :: TranspileResult (ContractPart SourceRange) IContractBodyElement' (Maybe (Scr.Param Ann)) <- transpile sol ""
  return (scryptCode tr, transpileLogs tr)


transpileParameter :: String -> IO (String, Logs)
transpileParameter sol = do
  tr :: TranspileResult (Parameter SourceRange) IParam' (Maybe (Scr.Param Ann)) <- transpile sol ""
  return (scryptCode tr, transpileLogs tr)

transpileStatic :: String -> IO (String, Logs)
transpileStatic sol = do
  tr :: TranspileResult (ContractPart SourceRange) IContractBodyElement' (Maybe (Scr.Static Ann)) <- transpile sol ""
  return (scryptCode tr, transpileLogs tr)

spec :: IO TestTree
spec = testSpec "Transpile Variable" $ do

  let itProperty sol scrypt = it ("should transpile Solidity `" ++ sol ++ "` correctly") $ do
        tr  <- transpileProperty sol
        tr `shouldBe` (scrypt, [])
  let itParameter sol scrypt = it ("should transpile Solidity `" ++ sol ++ "` correctly") $ do
        tr  <- transpileParameter sol
        tr `shouldBe` (scrypt, [])
  let itStatic sol scrypt = it ("should transpile Solidity `" ++ sol ++ "` correctly") $ do
        tr  <- transpileStatic sol
        tr `shouldBe` (scrypt, [])

  let itPropertyReportError sol errs = it ("should report error when transpiling Solidity Property `" ++ sol ++ "`") $ do
        (code, logs) <- transpileProperty sol
        code `shouldBe` ""
        logs `shouldBe` map (uncurry (Log ErrorLevel)) errs

  let itParameterReportError sol errs = it ("should report error when transpiling Solidity Parameter `" ++ sol ++ "`") $ do
        (code, logs) <- transpileParameter sol
        code `shouldBe` ""
        logs `shouldBe` map (uncurry (Log ErrorLevel)) errs

  let itStaticReportError sol errs = it ("should report error when transpiling Solidity constant Property `" ++ sol ++ "`") $ do
        (code, logs) <- transpileStatic sol
        code `shouldBe` ""
        logs `shouldBe` map (uncurry (Log ErrorLevel)) errs

  describe "#Property" $ do
      itProperty "uint storedData;" "\n@state\nint storedData;"
      itProperty "int storedData;" "\n@state\nint storedData;"
      itProperty "bool a;" "\n@state\nbool a;"
      itProperty "bytes a;" "\n@state\nbytes a;"
      itProperty "bytes private a;" "\n@state\nprivate bytes a;"
      itProperty "bytes public a;" "\n@state\npublic bytes a;"
      itProperty "string public a;" "\n@state\npublic bytes a;"
      itProperty "address public a;" "\n@state\npublic PubKeyHash a;"
      itPropertyReportError "uint aa = 3;" 
        [("unsupported state variable with init value", newSR (1, 1) (1, 13))]
      itPropertyReportError "uint[] aa;" 
        [("array length should be explicitly specified", newSR (1, 1) (1, 7))]
      itPropertyReportError "address immutable owner = msg.sender;" 
        [("unsupported state variable with init value", newSR (1, 1) (1, 38))]
      itPropertyReportError "address payable a;" 
        [("unsupported type `TypeNameElementaryTypeName`", newSR (1, 1) (1, 16))]  
  describe "#Parameter" $ do
    itParameter "int a" "int a"
    itParameter "uint a" "int a"
    itParameter "byte a" "bytes a"
    itParameter "bytes a" "bytes a"
    itParameter "string a" "bytes a"
    itParameter "address a" "PubKeyHash a"
    itParameter "bool a" "bool a"
    itParameterReportError "bytes[] memory x" 
        [("array length should be explicitly specified", newSR (1, 1) (1, 8))]
    itParameterReportError "address payable a" 
        [("unsupported type `TypeNameElementaryTypeName`", newSR (1, 1) (1, 16))] 
  describe "#Static" $ do
    itStatic "uint constant x = 1;" "\nstatic const int x = 1;"
    itStatic "uint constant x = 1 + 1 *(1-1);" "\nstatic const int x = 1 + 1 * (1 - 1);"
    itStatic "bool constant x = true;" "\nstatic const bool x = true;"
    itStatic "int constant internal INT_TWO = 2;" "\nstatic const int INT_TWO = 2;"
    itStaticReportError "int constant internal UINT_MIN = int(2 ** 4);" 
        [("unsupported binary operator `**`", newSR (1, 40) (1, 42))] 
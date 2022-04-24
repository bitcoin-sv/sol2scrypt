{-# LANGUAGE ScopedTypeVariables #-}

module TypeSpec where

import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transpiler
import Helper

transpileSol :: String -> IO (String, Logs)
transpileSol sol = do
  tr :: TranspileResult (TypeName SourceRange) IType' (Maybe Type) <- transpile sol ""
  return (scryptCode tr, transpileLogs tr)

spec :: IO TestTree
spec = testSpec "Transpile Type" $ do
  let itType sol scrypt = it ("should transpile Solidity `" ++ sol ++ "` correctly") $ do
        tr :: TranspileResult (TypeName SourceRange) IType' (Maybe Type) <- transpile sol ""
        scryptCode tr `shouldBe` scrypt

  
  let itReportError sol err colRange = it ("should report error when transpiling Solidity `" ++ sol ++ "`") $ do
        (code, logs) <- transpileSol sol
        code `shouldBe` ""
        logs `shouldBe` [Log ErrorLevel err $ firstLineSR colRange]

  describe "#ElementaryTypeName" $ do
    describe "#BoolType" $ do
      itType "bool" "bool"

    describe "#UintType" $ do

      itType "uint256" "int"
      
      itType "uint248" "int"

      itType "uint240" "int"

      itType "uint232" "int"

      itType "uint224" "int"

      itType "uint216" "int"

      itType "uint208" "int"

      itType "uint200" "int"

      itType "uint192" "int"

      itType "uint184" "int"

      itType "uint176" "int"

      itType "uint168" "int"

      itType "uint160" "int"

      itType "uint152" "int"

      itType "uint144" "int"

      itType "uint136" "int"

      itType "uint128" "int"

      itType "uint120" "int"

      itType "uint112" "int"

      itType "uint104" "int"

      itType "uint96" "int"

      itType "uint88" "int"

      itType "uint72" "int"

      itType "uint64" "int"

      itType "uint56" "int"

      itType "uint48" "int"

      itType "uint40" "int"

      itType "uint32" "int"

      itType "uint24" "int"

      itType "uint16" "int"

      itType "uint8" "int"

      itType "uint" "int"


    describe "#IntType" $ do

      itType "int256" "int"
      
      itType "int248" "int"

      itType "int240" "int"

      itType "int232" "int"

      itType "int224" "int"

      itType "int216" "int"

      itType "int208" "int"

      itType "int200" "int"

      itType "int192" "int"

      itType "int184" "int"

      itType "int176" "int"

      itType "int168" "int"

      itType "int160" "int"

      itType "int152" "int"

      itType "int144" "int"

      itType "int136" "int"

      itType "int128" "int"

      itType "int120" "int"

      itType "int112" "int"

      itType "int104" "int"

      itType "int96" "int"

      itType "int88" "int"
      
      itType "int80" "int"

      itType "int72" "int"

      itType "int64" "int"

      itType "int56" "int"

      itType "int48" "int"

      itType "int40" "int"

      itType "int32" "int"

      itType "int24" "int"

      itType "int16" "int"

      itType "int8" "int"

      itType "int" "int"

    describe "#ByteType" $ do

      itType "byte" "bytes"

      itType "bytes" "bytes"

      itType "bytes1" "bytes"

      itType "bytes2" "bytes"

      itType "bytes3" "bytes"

      itType "bytes4" "bytes"

      itType "bytes5" "bytes"

      itType "bytes6" "bytes"

      itType "bytes7" "bytes"

      itType "bytes8" "bytes"

      itType "bytes9" "bytes"

      itType "bytes10" "bytes"

      itType "bytes11" "bytes"

      itType "bytes12" "bytes"

      itType "bytes13" "bytes"

      itType "bytes14" "bytes"

      itType "bytes15" "bytes"

      itType "bytes16" "bytes"

      itType "bytes17" "bytes"

      itType "bytes18" "bytes"

      itType "bytes19" "bytes"

      itType "bytes20" "bytes"

      itType "bytes21" "bytes"

      itType "bytes22" "bytes"

      itType "bytes23" "bytes"

      itType "bytes24" "bytes"

      itType "bytes25" "bytes"
      
      itType "bytes26" "bytes"

      itType "bytes27" "bytes"

      itType "bytes28" "bytes"

      itType "bytes29" "bytes"

      itType "bytes30" "bytes"

      itType "bytes31" "bytes"
      
      itType "bytes32" "bytes"

    describe "#Address" $ do

      itType "address" "PubKeyHash"

    describe "#VarType" $ do

      itType "var" "auto"

    describe "#StringType" $ do
      itType "string" "bytes"

  describe "#TypeNameArrayTypeName" $ do

    itType "int[20]" "int[20]"
    itType "uint[20]" "int[20]"
    itType "bool[20]" "bool[20]"
    itType "bytes[20] " "bytes[20]"

  describe "#TypeNameMapping" $ do
    itType "mapping (address => uint)" "HashedMap<PubKeyHash, int>"
    itType "mapping (address => mapping (address => uint))" "HashedMap<MapKeyST0, int>"
    itType "mapping (address => bool)" "HashedMap<PubKeyHash, bool>"
    itType "mapping (address => bytes)" "HashedMap<PubKeyHash, bytes>"
  describe "#unsupported type should report error1" $ do
    itReportError "aaaE " "unsupported type: `aaaE`" (1, 5)
    itReportError "uint []  " "array length should be explicitly specified" (1, 8)
    itReportError "bool [2] []  " "array length should be explicitly specified" (1, 12)
     

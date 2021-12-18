{-# LANGUAGE ScopedTypeVariables #-}

module TypeSpec where

import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transpiler

spec :: IO TestTree
spec = testSpec "Transpile Type" $ do
  describe "#ElementaryTypeName" $ do
    describe "#BoolType" $ do
      it "should transpile Solidity `bool` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bool"
        scryptCode tr `shouldBe` "bool"

    describe "#UintType" $ do

      it "should transpile Solidity `uint256` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint256"
        scryptCode tr `shouldBe` "int"
      
      it "should transpile Solidity `uint248` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint248"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint240` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint240"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint232` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint232"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint224` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint224"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint216` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint216"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint208` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint208"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint200` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint200"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint192` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint192"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint184` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint184"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint176` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint176"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint168` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint168"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint160` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint160"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint152` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint152"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint144` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint144"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint136` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint136"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint128` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint128"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint120` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint120"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint112` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint112"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint104` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint104"
        scryptCode tr `shouldBe` "int"
      it "should transpile Solidity `uint96` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint96"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint88` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint88"
        scryptCode tr `shouldBe` "int"
      it "should transpile Solidity `uint80` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint80"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint72` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint72"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint64` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint64"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint56` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint56"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint48` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint48"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint40` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint40"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint32` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint32"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint24` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint24"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint16` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint16"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint8` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint8"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `uint` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint"
        scryptCode tr `shouldBe` "int"


    describe "#IntType" $ do

      it "should transpile Solidity `int256` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int256"
        scryptCode tr `shouldBe` "int"
      
      it "should transpile Solidity `int248` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int248"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int240` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int240"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int232` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int232"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int224` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int224"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int216` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int216"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int208` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int208"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int200` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int200"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int192` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int192"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int184` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int184"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int176` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int176"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int168` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int168"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int160` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int160"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int152` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int152"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int144` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int144"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int136` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int136"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int128` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int128"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int120` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int120"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int112` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int112"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int104` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int104"
        scryptCode tr `shouldBe` "int"
      it "should transpile Solidity `int96` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int96"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int88` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int88"
        scryptCode tr `shouldBe` "int"
      it "should transpile Solidity `int80` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int80"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int72` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int72"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int64` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int64"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int56` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int56"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int48` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int48"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int40` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int40"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int32` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int32"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int24` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int24"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int16` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int16"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int8` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int8"
        scryptCode tr `shouldBe` "int"

      it "should transpile Solidity `int` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "int"
        scryptCode tr `shouldBe` "int"


    describe "#ByteType" $ do

      it "should transpile Solidity `byte` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "byte"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes1` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes1"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes2` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes2"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes3` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes3"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes4` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes4"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes5` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes5"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes6` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes6"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes7` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes7"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes8` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes8"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes9` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes9"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes10` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes10"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes11` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes11"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes12` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes12"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes13` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes13"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes14` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes14"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes15` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes15"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes16` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes16"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes17` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes17"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes18` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes18"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes19` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes19"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes20` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes20"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes21` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes21"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes22` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes22"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes23` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes23"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes24` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes24"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes25` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes25"
        scryptCode tr `shouldBe` "bytes"
      
      it "should transpile Solidity `bytes26` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes26"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes27` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes27"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes28` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes28"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes29` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes29"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes30` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes30"
        scryptCode tr `shouldBe` "bytes"

      it "should transpile Solidity `bytes31` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes31"
        scryptCode tr `shouldBe` "bytes"
      
      it "should transpile Solidity `bytes32` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "bytes32"
        scryptCode tr `shouldBe` "bytes"

    describe "#Address" $ do

      it "should transpile Solidity `address` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "address"
        scryptCode tr `shouldBe` "Ripemd160"

    describe "#VarType" $ do

      it "should transpile Solidity `var` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "var"
        scryptCode tr `shouldBe` "auto"
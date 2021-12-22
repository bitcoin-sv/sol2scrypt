{-# LANGUAGE ScopedTypeVariables #-}

module StmtSpec where
import Transpiler
import Solidity as Sol
import IR
import Scrypt as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils

spec :: IO TestTree
spec = testSpec "Transpile Statement" $ do

  let itstmt title sol scrypt = it ("should transpile Solidity `" ++ title ++ "` correctly") $ do
        tr :: TranspileResult Sol.Statement IStatement' (Maybe (Scr.Statement IExpr)) <- transpile sol
        scryptCode tr `shouldBe` scrypt

  describe "#SimpleStatementExpression" $ do
    describe "#NumberLiteral" $ do
      itstmt "NumberLiteralHex"  "0x123a;"  "0x123a;"
      itstmt "NumberLiteralDec"  "256;"  "256;"

    describe "#HexLiteral" $ do
      itstmt "HexLiteral"  "hex\"010113\";"  "b'010113';"
      itstmt "HexLiteral empty"  "hex\"\";"  "b'';"

    describe "#Parans" $ do
      itstmt "Parans number"  "(0);"  "(0);"
      -- itstmt "Parans bool"  "(true);"  "(true);"

    describe "#Unary" $ do
      itstmt "++"  "a++;"  "a++;"
      itstmt "--"  "--a;"  "--a;"
      itstmt "!"  "!a;"  "!a;"

    describe "#Binary" $ do
      itstmt "+"  "1+3;"  "1 + 3;"
      itstmt "-"  "a - 1;"  "a - 1;"
      itstmt ">="  "a >= 1;"  "a >= 1;"

    describe "#SimpleStatementVariableAssignmentList" $ do
      itstmt "assignment"  "x = 11;"  "x = 11;"

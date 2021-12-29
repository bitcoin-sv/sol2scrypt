{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module StmtSpec where
import Transpiler
import Solidity as Sol
import IR
import Scrypt as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Text.RawString.QQ
import Utils

spec :: IO TestTree
spec = testSpec "Transpile Statement" $ do

  let itstmt title sol scrypt = it ("should transpile Solidity `" ++ title ++ "` correctly") $ do
        tr :: TranspileResult Sol.Statement IStatement' (Maybe (Scr.Statement Ann)) <- transpile sol
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
    itstmt "assignment"  "x = uint(11);"  "x = uint(11);"
    itstmt "assignment"  "x = a;"  "x = a;"
    itstmt "assignment"  "x = a + 1;"  "x = a + 1;"
    itstmt "assignment"  "x = true;"  "x = true;"
    itstmt "assignment"  "x = false;"  "x = false;"
    itstmt "assignment"  "x = a || b;"  "x = a || b;"
    itstmt "assignment"  "x = a && b;"  "x = a && b;"
    itstmt "assignment"  "x = ((a+b)*4) && (b/23 == 0);"  "x = ((a + b) * 4) && (b / 23 == 0);"
    

  describe "#SimpleStatementVariableDeclarationList" $ do
    itstmt "int"  "int x = 11;"  "int x = 11;"
    itstmt "int"  "int x = 1 + 2;"  "int x = 1 + 2;"
    itstmt "int"  "int x = a + b;"  "int x = a + b;"
    itstmt "int"  "int x = -1;"  "int x = -1;"
    itstmt "int"  "int x = -(0x1);"  "int x = -(0x1);"
    itstmt "int"  "int x = -0x1;"  "int x = -(0x1);"
    itstmt "uint"  "uint x = 11;"  "int x = 11;"
    itstmt "uint"  "uint s = sum(_arr);"  "int s = sum(_arr);"
    itstmt "bool"  "bool x = true;"  "bool x = true;"
    itstmt "bytes"  "bytes x = hex\"010113\";"  "bytes x = b'010113';"

    describe "#BlockStatement" $ do
      itstmt "BlockStatement"  "{bytes x = hex\"010113\";}"  "{bytes x = b'010113';}"
      itstmt "BlockStatement"  "{1 + 2;}"  "{1 + 2;}"
      itstmt "BlockStatement"  "{1;}"  "{1;}"
      itstmt "BlockStatement"  "{}"  "{}"
      itstmt "BlockStatement"  [r|{
        1 + 2;
        int x = 3;
        x = x * 4 + 1;
      }|]   "{1 + 2; int x = 3; x = x * 4 + 1;}"

      itstmt "BlockStatement"  [r|{
        count += 1;
        bytes x = hex"010113";
        address nameReg = 0xdCad3a6d3569DF655070DEd06cb7A1b2Ccd1D3AF;
        count++;
        count--;
        --count;
        ++count;
        bool a = true;
        bool b = !a;
      }|]   "{count += 1; bytes x = b'010113'; Ripemd160 nameReg = 0xdcad3a6d3569df655070ded06cb7a1b2ccd1d3af; count++; count--; --count; ++count; bool a = true; bool b = !a;}"

      

    describe "#Sol Parser bug? #9" $ do
      itstmt "SimpleStatementExpression"  "true;"  "true;"
      itstmt "SimpleStatementExpression"  "false;"  "false;"
      itstmt "SimpleStatementExpression"  "false || true;"  "false || true;"
      itstmt "SimpleStatementExpression"  "false && true;"  "false && true;"
      itstmt "BlockStatement"  [r|{
        true;
        false;
        false || true;
        false && true;
      }|]   "{true; false; false || true; false && true;}"


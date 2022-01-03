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
      itstmt "NumberLiteralHex"  "0x123a;"  "\n0x123a;"
      itstmt "NumberLiteralDec"  "256;"  "\n256;"

    describe "#HexLiteral" $ do
      itstmt "HexLiteral"  "hex\"010113\";"  "\nb'010113';"
      itstmt "HexLiteral empty"  "hex\"\";"  "\nb'';"

    describe "#Parans" $ do
      itstmt "Parans number"  "(0);"  "\n(0);"
      itstmt "Parans bool"  "(true);"  "\n(true);"

    describe "#Unary" $ do
      itstmt "++"  "a++;"  "\na++;"
      itstmt "--"  "--a;"  "\n--a;"
      itstmt "!"  "!a;"  "\n!a;"

    describe "#Binary" $ do
      itstmt "+"  "1+3;"  "\n1 + 3;"
      itstmt "-"  "a - 1;"  "\na - 1;"
      itstmt ">="  "a >= 1;"  "\na >= 1;"

  describe "#SimpleStatementVariableAssignmentList" $ do
    itstmt "assignment"  "x = 11;"  "\nx = 11;"
    itstmt "assignment"  "x = uint(11);"  "\nx = uint(11);"
    itstmt "assignment"  "x = a;"  "\nx = a;"
    itstmt "assignment"  "x = a + 1;"  "\nx = a + 1;"
    itstmt "assignment"  "x = true;"  "\nx = true;"
    itstmt "assignment"  "x = false;"  "\nx = false;"
    itstmt "assignment"  "x = a || b;"  "\nx = a || b;"
    itstmt "assignment"  "x = a && b;"  "\nx = a && b;"
    itstmt "assignment"  "x = ((a+b)*4) && (b/23 == 0);"  "\nx = ((a + b) * 4) && (b / 23 == 0);"
    

  describe "#SimpleStatementVariableDeclarationList" $ do
    itstmt "int"  "int x = 11;"  "\nint x = 11;"
    itstmt "int"  "int x = 1 + 2;"  "\nint x = 1 + 2;"
    itstmt "int"  "int x = a + b;"  "\nint x = a + b;"
    itstmt "int"  "int x = -1;"  "\nint x = -1;"
    itstmt "int"  "int x = -(0x1);"  "\nint x = -(0x1);"
    itstmt "int"  "int x = -0x1;"  "\nint x = -(0x1);"
    itstmt "uint"  "uint x = 11;"  "\nint x = 11;"
    itstmt "uint"  "uint s = sum(_arr);"  "\nint s = sum(_arr);"
    itstmt "bool"  "bool x = true;"  "\nbool x = true;"
    itstmt "bytes"  "bytes x = hex\"010113\";"  "\nbytes x = b'010113';"

    describe "#BlockStatement" $ do
      itstmt "BlockStatement"  "{bytes x = hex\"010113\";}"  "\n{\n  bytes x = b'010113';\n}"
      itstmt "BlockStatement"  "{1 + 2;}"  "\n{\n  1 + 2;\n}"
      itstmt "BlockStatement"  "{1;}"  "\n{\n  1;\n}"
      itstmt "BlockStatement"  "{}"  "\n{\n}"
      itstmt "BlockStatement"  [r|{
        1 + 2;
        int x = 3;
        x = x * 4 + 1;
      }|] [r|
{
  1 + 2;
  int x = 3;
  x = x * 4 + 1;
}|]

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
      }|] [r|
{
  count += 1;
  bytes x = b'010113';
  Ripemd160 nameReg = 0xdcad3a6d3569df655070ded06cb7a1b2ccd1d3af;
  count++;
  count--;
  --count;
  ++count;
  bool a = true;
  bool b = !a;
}|]

      

    describe "#Fix Sol Parser bug #9" $ do
      itstmt "SimpleStatementExpression"  "true;"  "\ntrue;"
      itstmt "SimpleStatementExpression"  "false;"  "\nfalse;"
      itstmt "SimpleStatementExpression"  "(false);"  "\n(false);"
      itstmt "SimpleStatementExpression"  "false || true;"  "\nfalse || true;"
      itstmt "SimpleStatementExpression"  "false && true;"  "\nfalse && true;"
      itstmt "BlockStatement"  [r|{
        true;
        false;
        false || true;
        false && true;
      }|] [r|
{
  true;
  false;
  false || true;
  false && true;
}|]

  describe "#indent" $ do
    describe "#should indent when printing block statement." $ do
      itstmt "SimpleStatementExpression"  "{ true;{ true; {true; a=a+1; {a++;}}}}"  
        [r|
{
  true;
  {
    true;
    {
      true;
      a = a + 1;
      {
        a++;
      }
    }
  }
}|]




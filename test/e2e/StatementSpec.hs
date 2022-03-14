{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module StatementSpec where
import Transpiler
import Solidity as Sol
import IR
import Scrypt as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Text.RawString.QQ
import Utils
import Helper

transpileSol :: String -> IO (String, Logs)
transpileSol sol = do
  tr :: TranspileResult (Sol.Statement SourceRange) IStatement' (Maybe (Scr.Statement Ann)) <- transpile sol ""
  return (scryptCode tr, transpileLogs tr)

spec :: IO TestTree
spec = testSpec "Transpile Statement" $ do

  let itstmt title sol scrypt = it ("should transpile Solidity `" ++ title ++ "` correctly") $ do
        tr  <- transpileSol sol
        tr `shouldBe` (scrypt, [])
        
  let itReportError sol errs = it ("should report error when transpiling Solidity Statement `" ++ sol ++ "`") $ do
        (code, logs) <- transpileSol sol
        code `shouldBe` ""
        logs `shouldBe` map (\(e, colRange) -> Log ErrorLevel e $ firstLineSR colRange) errs

  let itMultiStmts title sol scrypt = it ("should transpile Solidity `" ++ title ++ "` correctly") $ do
        tr :: TranspileResult (Sol.Statement SourceRange) [IStatement'] [Maybe (Scr.Statement Ann)] <- transpile sol ""
        (scryptCode tr, transpileLogs tr) `shouldBe` (scrypt, [])

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
    itstmt "assignment"  "x = uint(11);"  "\nx = 11;"
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
    itstmt "string"  "string x = \"abc0123\";"  "\nbytes x = \"abc0123\";"
    itstmt "string"  "string memory message = string(\"aa\");"  "\nbytes message = \"aa\";"
    itstmt "new bytes"  "bytes memory bstr = new bytes(10);"  "\nbytes bstr = num2bin(0, 10);"

  describe "#require" $ do
    itstmt "require"  "require(true);"  "\nrequire(true);"
    itstmt "require"  "require(true, \"\");"  "\nrequire(true);"
    itstmt "require"  "require(owner != sender, \"ERC20: approve from the zero address\");"  "\nrequire(owner != sender);"
    itstmt "require"  "require((Age > 10) && (Age < 20) || (Age > 40) && (Age < 50), \"ERC20: approve from the zero address\");"  "\nrequire((Age > 10) && (Age < 20) || (Age > 40) && (Age < 50));"
    itstmt "assert"  "assert(true);"  "\nrequire(true);"     
    itstmt "assert"  "assert(a == 3);"  "\nrequire(a == 3);"   

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
  PubKeyHash nameReg = 0xdcad3a6d3569df655070ded06cb7a1b2ccd1d3af;
  count++;
  count--;
  --count;
  ++count;
  bool a = true;
  bool b = !a;
}|]

    itstmt "BlockStatement with return"
        "{ if(x) {return y;} return z; }"
        [r|
{
  if (x) {
    {
      ret = y;
      returned = true;
    }
  }
  return returned ? ret : z;
}|]

    itstmt "BlockStatement with embedded if & return"
      [r|{
  uint x = 3;
  if(x > 0) {
      if (x > 1) {
          x /= 2;
          if(x == 2) {
              return x;
          }
          x--;
          if(x == 3) {
              return x;
          } else {
              x += y;
          }
          x += 2 * amount + 1;
      }
      x++;
      x += 11;
  } else {
      --x;
  }
  x = x + amount;
  x += 20 / amount - 12;
  return x;
}|]
      [r|
{
  int x = 3;
  if (x > 0) {
    if (x > 1) {
      x /= 2;
      if (x == 2) {
        {
          ret = x;
          returned = true;
        }
      }
      if (!returned) {
        x--;
        if (x == 3) {
          {
            ret = x;
            returned = true;
          }
        }
        else {
          x += y;
        }
        if (!returned) {
          x += 2 * amount + 1;
        }
      }
    }
    if (!returned) {
      x++;
      x += 11;
    }
  }
  else {
    --x;
  }
  if (!returned) {
    x = x + amount;
    x += 20 / amount - 12;
  }
  return returned ? ret : x;
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

  describe "#IfStmt" $ do
    itstmt "If stmt with block stmt in true branch, without else"  "if(true){true;}"  
      [r|
if (true) {
  true;
}|]


    itstmt "If stmt with expression stmt in true branch, without else"  "if(true) true;"  "\nif (true)\n  true;"

    itstmt "If statement with else"  "if(true){true;}else{false;}"  
      [r|
if (true) {
  true;
}
else {
  false;
}|]

    itstmt "If statement with multi else"  
      [r|if (true) {
    1 + 3;
} else if (1 == a) {
    a++;
} else if (!c != (c || d)) {
    e = b * 2;
} else {
    int x  = 3;
}|]
      [r|
if (true) {
  1 + 3;
}
else if (1 == a) {
  a++;
}
else if (!c != (c || d)) {
  e = b * 2;
}
else {
  int x = 3;
}|]

    itstmt "If statement with nested if/else"  
      [r|if (true) {
  if (true) {
    a++;
    if (c) {
      --a;
    } else {
      a--;
    }
  } else {
    a--;
  }

} else {
    int x  = 3;
    if(false) {
      a++;
    } else {
      a--;
    }
}|]
      [r|
if (true) {
  if (true) {
    a++;
    if (c) {
      --a;
    }
    else {
      a--;
    }
  }
  else {
    a--;
  }
}
else {
  int x = 3;
  if (false) {
    a++;
  }
  else {
    a--;
  }
}|]

    itstmt "If stmt with assign stmt in true branch, no else"  "if (x > 0) storedData = x +1;"
       [r|
if (x > 0)
  storedData = x + 1;|]

    itstmt "If stmt with assign stmt in true branch, with else"  "if (x > 0) storedData = x +1; else { uint a = 3; }"
       [r|
if (x > 0)
  storedData = x + 1;
else {
  int a = 3;
}|]

    itstmt "If stmt with declare stmt in true branch, no else"  "if (x > 0) int x = x3 +1;"
       [r|
if (x > 0)
  int x = x3 + 1;|]

    itstmt "If stmt with declare stmt in true branch, with else"  "if (x > 0) int x = x3 +1; else uint a = 3;"
       [r|
if (x > 0)
  int x = x3 + 1;
else
  int a = 3;|]

  describe "#PlaceholderStatement" $ do
      itstmt "PlaceholderStatement" "_;" ""

  describe "#ReportError" $ do
    itReportError "D newD = new D(1);" [("unsupported expression : `New`", (10, 15)), 
      ("unsupported type `TypeNameUserDefinedTypeName`", (1, 2)),
      ("unsupported SimpleStatementVariableDeclarationList", (1, 19))]
    itReportError "a[i] = 3;" [("unsupported assign Statement, subscript cannot be a variable", (1, 10))]
    itReportError "assembly { let size := extcodesize(_addr) }" [("unsupported statement `InlineAssemblyStatement`", (1, 44))]

  describe "#ForStatement" $ do

    itMultiStmts "simple ForStatement"
      [r|for(uint i=0; i<2; i++){
        if (i>1) {
          a = b;
        }
      }
|]
      [r|
int i = 0;
loop (__LoopCount__0) {
  if (i < 2) {
    if (i > 1) {
      a = b;
    }
    i++;
  }
}|]

    itMultiStmts "simple ForStatement with empty exprs"
      [r|for(;;){
        if (i>1) {
          a = b;
        }
      }
|]
      [r|
loop (__LoopCount__0) {
  if (true) {
    if (i > 1) {
      a = b;
    }
  }
}|]

    itMultiStmts "simple ForStatement with break"
      [r|for (j = _i; j != 0; j /= 10) {
        if (i>1) {
          break;
        }
        i++;
      }
|]
      [r|
bool loopBreakFlag0 = false;
j = _i;
loop (__LoopCount__0) {
  if (!loopBreakFlag0 && j != 0) {
    if (i > 1) {
      loopBreakFlag0 = true;
    }
    if (!loopBreakFlag0) {
      i++;
    }
    if (!loopBreakFlag0) {
      j /= 10;
    }
  }
}|]

    itMultiStmts "simple ForStatement with continue"
      [r|for (j = _i; j != 0; j /= 10) {
        if (j>1) {
          continue;
        }
        j += _i;
      }
|]
      [r|
j = _i;
loop (__LoopCount__0) {
  if (j != 0) {
    bool loopContinueFlag0 = false;
    if (j > 1) {
      loopContinueFlag0 = true;
    }
    if (!loopContinueFlag0) {
      j += _i;
    }
    j /= 10;
  }
}|]

    itMultiStmts "simple ForStatement with multiple continue"
      [r|for (j = _i; j != 0; j /= 10) {
        if (j>1) {
          if (j > 2) {
            continue;
          }
          j+=3;
        }

        if (j<10) {
          j = 10;
          continue;
        }
        
        j += _i;
        j -= 10;
        j *= 2;
      }
|]
      [r|
j = _i;
loop (__LoopCount__0) {
  if (j != 0) {
    bool loopContinueFlag0 = false;
    if (j > 1) {
      if (j > 2) {
        loopContinueFlag0 = true;
      }
      if (!loopContinueFlag0) {
        j += 3;
      }
    }
    if (!loopContinueFlag0) {
      if (j < 10) {
        j = 10;
        loopContinueFlag0 = true;
      }
      if (!loopContinueFlag0) {
        j += _i;
        j -= 10;
        j *= 2;
      }
    }
    j /= 10;
  }
}|]

    itMultiStmts "simple ForStatement with return & continue"
      [r|for (j = _i; j != 0; j /= 10) {
        if (j>1) {
          return j;
        }
        if (j<3) {
          continue;
        }
        j += _i;
      }
|]
      [r|
j = _i;
loop (__LoopCount__0) {
  if (!returned && j != 0) {
    bool loopContinueFlag0 = false;
    if (j > 1) {
      {
        ret = j;
        returned = true;
      }
    }
    if (!returned) {
      if (j < 3) {
        loopContinueFlag0 = true;
      }
      if (!loopContinueFlag0) {
        j += _i;
      }
    }
    j /= 10;
  }
}|]

    itMultiStmts "simple ForStatement with break & continue"
      [r|for (j = _i; j != 0; j /= 10) {
        if (j>1) {
          if (j < 3) {
            break;
          }
          _i += 1;
          if (j<5) {
            continue;
          }
        }
        j += _i;
      }
|]
      [r|
bool loopBreakFlag0 = false;
j = _i;
loop (__LoopCount__0) {
  if (!loopBreakFlag0 && j != 0) {
    bool loopContinueFlag0 = false;
    if (j > 1) {
      if (j < 3) {
        loopBreakFlag0 = true;
      }
      if (!loopBreakFlag0) {
        _i += 1;
        if (j < 5) {
          loopContinueFlag0 = true;
        }
      }
    }
    if (!loopBreakFlag0 && !loopContinueFlag0) {
      j += _i;
    }
    if (!loopBreakFlag0) {
      j /= 10;
    }
  }
}|]

    itMultiStmts "embedded ForStatement with multiple continue"
      [r|for (j = _i; j != 0; j /= 10) {
        if (j>1) {
          continue;
        }

        for(uint i=0; i < j; i++) {
          if (i>4) {
            continue;
          }
          _i += 2;
        }
        
        j += _i;
        j -= 10;
      }
|]
      [r|
j = _i;
loop (__LoopCount__0) {
  if (j != 0) {
    bool loopContinueFlag0 = false;
    if (j > 1) {
      loopContinueFlag0 = true;
    }
    if (!loopContinueFlag0) {
      int i = 0;
      loop (__LoopCount__1) {
        if (i < j) {
          bool loopContinueFlag1 = false;
          if (i > 4) {
            loopContinueFlag1 = true;
          }
          if (!loopContinueFlag1) {
            _i += 2;
          }
          i++;
        }
      }
      j += _i;
      j -= 10;
    }
    j /= 10;
  }
}|]

    itMultiStmts "embedded ForStatement"
      [r|for(uint i=0; i<2; i++){
        if (i>1) {
          for(uint j=0; j<2; j++){
            if (i > j) break;
          }
          break;
        }
      }
|]
      [r|
bool loopBreakFlag0 = false;
int i = 0;
loop (__LoopCount__0) {
  if (!loopBreakFlag0 && i < 2) {
    if (i > 1) {
      bool loopBreakFlag1 = false;
      int j = 0;
      loop (__LoopCount__1) {
        if (!loopBreakFlag1 && j < 2) {
          if (i > j) {
            loopBreakFlag1 = true;
          }
          if (!loopBreakFlag1) {
            j++;
          }
        }
      }
      loopBreakFlag0 = true;
    }
    if (!loopBreakFlag0) {
      i++;
    }
  }
}|]

  describe "#WhileStatement" $ do

    itMultiStmts "simple WhileStatement"
      [r|while(i<2){
        if (i>1) {
          a = b;
          i++;
        }
      }
|]
      [r|
loop (__LoopCount__0) {
  if (i < 2) {
    if (i > 1) {
      a = b;
      i++;
    }
  }
}|]

    itMultiStmts "simple WhileStatement with break"
      [r|while(i<2){
        if (i>1) {
          a = b;
          break;
        }
      }
|]
      [r|
bool loopBreakFlag0 = false;
loop (__LoopCount__0) {
  if (!loopBreakFlag0 && i < 2) {
    if (i > 1) {
      a = b;
      loopBreakFlag0 = true;
    }
  }
}|]

    itMultiStmts "simple WhileStatement with continue"
      [r|while(i<2){
        if (i>1) {
          continue;
        }
        a = b;
      }
|]
      [r|
loop (__LoopCount__0) {
  if (i < 2) {
    bool loopContinueFlag0 = false;
    if (i > 1) {
      loopContinueFlag0 = true;
    }
    if (!loopContinueFlag0) {
      a = b;
    }
  }
}|]

  describe "#DoWhileStatement" $ do

    itMultiStmts "simple DoWhileStatement"
      [r|do {
        if (i>1) {
          a = b;
        }
      } while(i<2);
|]
      [r|
{
  if (i > 1) {
    a = b;
  }
}
loop (__LoopCount__0) {
  if (i < 2) {
    if (i > 1) {
      a = b;
    }
  }
}|]

    itMultiStmts "simple DoWhileStatement with break"
      [r|do {
        if (i>1) {
          a = b;
          break;
        }
      } while(i<2);
|]
      [r|
{
  if (i > 1) {
    a = b;
  }
}
bool loopBreakFlag0 = false;
loop (__LoopCount__0) {
  if (!loopBreakFlag0 && i < 2) {
    if (i > 1) {
      a = b;
      loopBreakFlag0 = true;
    }
  }
}|]

    itMultiStmts "simple DoWhileStatement with continue"
      [r|do {
        if (i>1) {
          continue;
        }
        a = b;
      } while(i<2);
|]
      [r|
{
  if (i > 1) {
  }
  a = b;
}
loop (__LoopCount__0) {
  if (i < 2) {
    bool loopContinueFlag0 = false;
    if (i > 1) {
      loopContinueFlag0 = true;
    }
    if (!loopContinueFlag0) {
      a = b;
    }
  }
}|]

    itMultiStmts "nested DoWhileStatement & ForStatement"
      [r|for(uint i=0; i<10; i++){
  do {
    uint t = 2;
    if (i>1) {
      a = b;
      break;
    }
  } while(i<2);
  c = d + i;
}
|]
      [r|
int i = 0;
loop (__LoopCount__0) {
  if (i < 10) {
    {
      int t = 2;
      if (i > 1) {
        a = b;
      }
    }
    bool loopBreakFlag1 = false;
    loop (__LoopCount__1) {
      if (!loopBreakFlag1 && i < 2) {
        int t = 2;
        if (i > 1) {
          a = b;
          loopBreakFlag1 = true;
        }
      }
    }
    c = d + i;
    i++;
  }
}|]
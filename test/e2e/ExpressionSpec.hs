{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module ExpressionSpec where

import qualified Data.Map.Lazy as Map
import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transpiler
import Utils
import Helper

transpileSol :: String -> IO (String, Logs)
transpileSol sol = do
  tr :: TranspileResult (Expression SourceRange) IExpression' (Maybe (Expr Ann)) <- transpile sol ""
  return (scryptCode tr, transpileLogs tr)

spec :: IO TestTree
spec = testSpec "Transpile Expression" $ do
  let itexpr title sol scrypt = it ("should transpile Solidity `" ++ title ++ "` correctly") $ do
        tr :: TranspileResult (Expression SourceRange) IExpression' (Maybe (Expr Ann)) <- transpile sol ""
        scryptCode tr `shouldBe` scrypt
  
  let itReportError sol err colRange = it ("should report error when transpiling Solidity Expression `" ++ sol ++ "`") $ do
        (code, logs) <- transpileSol sol
        code `shouldBe` ""
        logs `shouldBe` [Log ErrorLevel err $ firstLineSR colRange]
  
  describe "#PrimaryExpression" $ do
    describe "Identifier" $ do
      itexpr "Identifier" "aZ_$0" "aZ__0"
      itexpr "Identifier" "a" "a"
      itexpr "Identifier" "Af" "Af"
      itexpr "Identifier" "SigHashPreimage" "userDefined_SigHashPreimage"

    describe "#BooleanLiteral" $ do
      itexpr "BooleanLiteral true" "true" "true"
      itexpr "BooleanLiteral false" "false" "false"

    describe "#NumberLiteral" $ do
      itexpr "NumberLiteralHex" "0x123a" "0x123a"
      itexpr "NumberLiteralDec" "255" "255"
      itexpr "NumberLiteralDec" "-255" "-255"

    describe "#HexLiteral" $ do
      itexpr "HexLiteral" "hex\"010113\"" "b'010113'"
      itexpr "HexLiteral" "hex\"0aAD\"" "b'0aad'"
      itexpr "empty HexLiteral" "hex\"\"" "b''"

    describe "#StringLiteral" $ do
      itexpr "ascii" "\"abcde12\"" "\"abcde12\""
      itexpr "unicode" "\"你好\"" "\"你好\""
      itexpr "emoji" "\"😊\"" "\"😊\""
      itexpr "empty" "\"\"" "\"\""

  describe "#Unary Expression" $ do
    describe "#Unary" $ do
      itexpr "-" "-(0xf)" "-(0xf)"
      itexpr "-" "-0xf" "-(0xf)"
      itexpr "-" "-1" "-1"
      itexpr "()" "(0xf)" "(0xf)"
      itexpr "()" "((-0xf))" "((-(0xf)))"
      itexpr "++" "a++" "a++"
      itexpr "++" "++a" "++a"
      itexpr "--" "--a" "--a"
      itexpr "--" "a--" "a--"
      itexpr "!" "!a" "!a"
      itexpr "!" "!true" "!true"

  describe "#Binary Expression" $ do
    let itBinary op = itexpr ("Binary:" ++ op) ("1 " ++ op ++ " 3") ("1 " ++ op ++ " 3")
    describe "#Binary" $ do
      itBinary "+"

      itBinary "-"

      itBinary "*"

      itBinary "/"

      itBinary "%"

      itexpr "+=" "a += 1" "a += 1"

      itexpr "-=" "a -= 1" "a -= 1"

      itexpr "*=" "a *= 1" "a *= 1"

      itexpr "/=" "a /= 1" "a /= 1"

      itexpr "Binary: &&" "true && false" "true && false"
      itexpr "Binary: ||" "true || false" "true || false"

      itBinary "!="

      itBinary "=="

      itBinary "<"

      itBinary "<="

      itBinary ">"

      itBinary ">="

      itexpr "|=" "b |= a" "b |= a"
      itexpr "&=" "b &= a" "b &= a"
      itexpr ">>" "b >> a" "b >> a"
      itexpr "<<" "b << a" "b << a"
      itexpr ">>=" "b >>= a" "b >>= a"
      itexpr "<<=" "b <<= a" "b <<= a"
      itexpr "^=" "b ^= a" "b ^= a"

      describe "operator `[]`" $ do
        let squareBracketExpr = \leftExpr rightExpr -> leftExpr ++ "[" ++ rightExpr ++ "]"

        describe "when the left expr is a mapping-typed var" $ do
          let itSBExpr leftExpr rightExpr scrypt = 
                it "should transpile correctly" $ do
                  let mapSym = Symbol (IR.Identifier leftExpr) (Mapping (ElementaryType Address) (ElementaryType IR.Int)) False
                      initEnv =  [Map.insert (IR.Identifier leftExpr) mapSym Map.empty]
                      sol = leftExpr ++ "[" ++ rightExpr ++ "]" 
                  tr :: TranspileResult (Expression SourceRange) IExpression' (Maybe (Expr Ann)) <- 
                          transpile' (TransformState initEnv Nothing Map.empty [] Map.empty) sol ""
                  scryptCode tr `shouldBe` scrypt

          itSBExpr "a" "0" "a_0"
          itSBExpr "a" "true" "a_True"
          itSBExpr "a" "'ab'" "a_ab"
          itSBExpr "a" "b" "a_b"
          itSBExpr "a" "msg.sender" "a_msgSender"
        
        describe "when the left expr is not a mapping-typed var" $ do
          itexpr "[]" (squareBracketExpr "a" "b") "a[b]"
          itexpr "[]" (squareBracketExpr "a" "b()") "a[b()]"
          itexpr "[]" (squareBracketExpr "a()" "b") "a()[b]"

  describe "#Ternary Expression" $ do
    itexpr "Ternary" "true ? 1 : 2" "true ? 1 : 2"
    itexpr "Ternary" "false ? (1 + a) : a++" "false ? (1 + a) : a++"
    itexpr "Ternary" "(true ? 1 == a : 2 == a) ? (1 + a) : a++" "(true ? 1 == a : 2 == a) ? (1 + a) : a++"

  describe "#MemberAccess" $ do
    itexpr "plain MemberAccess" "a.b" "a.b"
    itexpr "embeded MemberAccess" "a.b.c" "a.b.c"
    itexpr "msg.sender" "msg.sender" "msgSender"
    itexpr "msg.value" "msg.value" "msgValue"

  describe "#FunctionCallExpressionList" $ do
    itexpr "FunctionCallExpressionList with identifer expr as function name" "a(b, c)" "a(b, c)"
    itexpr "FunctionCallExpressionList with member-access expr as function name" "a.b(c, d)" "a.b(c, d)"

  describe "#Complex expression" $ do
    let itComplex sol scrypt = itexpr sol sol scrypt

    itComplex "(a + 1) * (b -1)" "(a + 1) * (b - 1)"
    itComplex "((Age > 10) && (Age < 20)) || ((Age > 40) && (Age < 50) )" "((Age > 10) && (Age < 20)) || ((Age > 40) && (Age < 50))"
    itComplex "(48 + _i % 10) * 1" "(48 + _i % 10) * 1"
    itComplex "a % 4 == 0" "a % 4 == 0"
    itComplex "now >= start + daysAfter * 1" "now >= start + daysAfter * 1"
    itComplex "now >= start + daysAfter * 1" "now >= start + daysAfter * 1"
    itComplex "a + b * c" "a + b * c"
    itComplex "a + (b * c)" "a + (b * c)"
    itComplex "(a + b) * c" "(a + b) * c"
    itComplex "a * c + b * c" "a * c + b * c"
    itComplex "amount <= msg.value / 2" "amount <= msgValue / 2"
    itComplex "uint(1)" "1"
    itComplex "bytes1(hex\"00\")" "b'00'"
    itComplex "a[i+1]" "a[i + 1]"

  describe "#PrimaryExpressionTupleExpression" $ do
    itexpr "number array" "[1,3,1,3]" "[1, 3, 1, 3]"
    itexpr "number array" "[1,-3,1,0,0,0]" "[1, -3, 1, 0, 0, 0]"
    itexpr "number array" "[1,-0x3,-11,0,0,0]" "[1, -(0x3), -11, 0, 0, 0]"
    itexpr "number array" "[1+3,1-3,1*3,1/3,1%3]" "[1 + 3, 1 - 3, 1 * 3, 1 / 3, 1 % 3]"
    itexpr "bool array" "[true, true, false]" "[true, true, false]"
    itexpr "bytes array" "[hex\"010113\", hex\"\"]" "[b'010113', b'']"

  describe "#New Expression" $ do
    itexpr "new bytes" "new bytes(7)" "num2bin(0, 7)"
  describe "#ReportError" $ do
    -- operator
    itReportError "1 & 1 " "unsupported binary operator `&`" (3, 4)
    itReportError "1 | 1 " "unsupported binary operator `|`" (3, 4)
    itReportError "1 ^ 1 " "unsupported binary operator `^`" (3, 4)
    itReportError "~a " "unsupported unary operator `~`" (1, 2)
    -- -- buildin function
    itReportError "new uint[](3) " "unsupported expression : `New`" (1, 11)
    itReportError "assert(true) " "unsupported function call : `assert`" (1, 13)
    itReportError "keccak256(a) " "unsupported function call : `keccak256`" (1, 13)
    itReportError "ecrecover(hash, v, r, s) " "unsupported function call : `ecrecover`" (1, 25)
    itReportError "addmod(4, 5, 3) " "unsupported function call : `addmod`" (1, 16)
    itReportError "mulmod(4, 5, 3) " "unsupported function call : `mulmod`" (1, 16)
    itReportError "revert(4, 5, 3) " "unsupported function call : `revert`" (1, 16)
    itReportError "selfdestruct(a) " "unsupported function call : `selfdestruct`" (1, 16)
    itReportError "type(int) .min" "unsupported function call : `type`" (1, 10)
    itReportError "gasleft() " "unsupported function call : `gasleft`" (1, 10)
    itReportError "blockhash(1) " "unsupported function call : `blockhash`" (1, 13)
    -- -- Time Units
    itReportError "1 seconds " "unsupported expression : `Literal`" (1, 10)
    itReportError "1 days " "unsupported expression : `Literal`" (1, 7)
    itReportError "1 wei " "unsupported expression : `Literal`" (1, 6)
    -- -- block
    itReportError "block.number " "unsupported expression: `block.number`" (1, 13)
    itReportError "block.timestamp " "unsupported expression: `block.timestamp`" (1, 16)
    itReportError "block.basefee " "unsupported expression: `block.basefee`" (1, 14)
    itReportError "block.chainid " "unsupported expression: `block.chainid`" (1, 14)
    itReportError "block.coinbase " "unsupported expression: `block.coinbase`" (1, 15)
    itReportError "msg.sig " "unsupported expression: `msg.sig`" (1, 8)
    itReportError "msg.gas " "unsupported expression: `msg.gas`" (1, 8)
    itReportError "msg.data " "unsupported expression: `msg.data`" (1, 9)
    itReportError "tx.origin " "unsupported expression: `tx.origin`" (1, 10)
    itReportError "abi.encodePacked (arr, \"AAAA\", \"BBBB\")" "unsupported expression: `abi.encodePacked`" (1, 17)
    -- -- tuple
    itReportError "(1, 2, 3, 4, 5) " "unsupported expression : `Literal`" (1, 16)

    
      

      
      

{-# LANGUAGE ScopedTypeVariables #-}

module ExpressionSpec where

import IR
import Scrypt as Scr
import Solidity as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transpiler
import Utils

spec :: IO TestTree
spec = testSpec "Transpile Expression" $ do
  let itexpr title sol scrypt = it ("should transpile Solidity `" ++ title ++ "` correctly") $ do
        tr :: TranspileResult Expression IExpression' (Maybe (Expr Ann)) <- transpile sol
        scryptCode tr `shouldBe` scrypt
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

  describe "#Ternary Expression" $ do
    itexpr "Ternary" "true ? 1 : 2" "true ? 1 : 2"
    itexpr "Ternary" "false ? (1 + a) : a++" "false ? (1 + a) : a++"
    itexpr "Ternary" "(true ? 1 == a : 2 == a) ? (1 + a) : a++" "(true ? 1 == a : 2 == a) ? (1 + a) : a++"

  describe "#MemberAccess" $ do
    itexpr "plain MemberAccess" "a.b" "a.b"
    itexpr "embeded MemberAccess" "a.b.c" "a.b.c"

  describe "#FunctionCallExpressionList" $ do
    itexpr "FunctionCallExpressionList with identifer expr as function name" "a(b, c)" "a(b, c)"
    itexpr "FunctionCallExpressionList with member-access expr as function name" "a.b(c, d)" "a.b(c, d)"

  describe "#Complex expression" $ do
    let itComplex sol scrypt = itexpr sol sol scrypt

    itComplex "(a + 1) * (b -1)" "(a + 1) * (b - 1)"
    itComplex "((Age > 10) && (Age < 20)) || ((Age > 40) && (Age < 50) )" "((Age > 10) && (Age < 20)) || ((Age > 40) && (Age < 50))"
    itComplex "(48 + _i % 10) * 1" "(48 + _i % 10) * 1"
    itComplex "a % 4 == 0" "a % 4 == 0"
    itComplex "now >= start + daysAfter * 1 days" "now >= start + daysAfter * 1"
    itComplex "now >= start + daysAfter * 1 days" "now >= start + daysAfter * 1"
    itComplex "a + b * c" "a + b * c"
    itComplex "a + (b * c)" "a + (b * c)"
    itComplex "(a + b) * c" "(a + b) * c"
    itComplex "a * c + b * c" "a * c + b * c"
    itComplex "amount <= msg.value / 2" "amount <= msgValue / 2"
    itComplex "uint(1)" "uint(1)"

  describe "#PrimaryExpressionTupleExpression" $ do
    itexpr "number array" "[1,3,1,3]" "[1, 3, 1, 3]"
    itexpr "number array" "[1,-3,1,0,0,0]" "[1, -3, 1, 0, 0, 0]"
    itexpr "number array" "[1,-0x3,-11,0,0,0]" "[1, -(0x3), -11, 0, 0, 0]"
    itexpr "number array" "[1+3,1-3,1*3,1/3,1%3]" "[1 + 3, 1 - 3, 1 * 3, 1 / 3, 1 % 3]"
    itexpr "bool array" "[true, true, false]" "[true, true, false]"
    itexpr "bytes array" "[hex\"010113\", hex\"\"]" "[b'010113', b'']"

      

      
      

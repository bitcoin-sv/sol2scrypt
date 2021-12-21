{-# LANGUAGE ScopedTypeVariables #-}

module ExpressionSpec where
import Transpiler
import Solidity as Sol
import IR
import Scrypt as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "Transpile Expression" $ do

  let itexpr title sol scrypt = it ("should transpile Solidity `" ++ title ++ "` correctly") $ do
          tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile sol
          scryptCode tr `shouldBe` scrypt
  describe "#PrimaryExpression" $ do
    describe "Identifier" $ do
      itexpr "Identifier" "aZ_$0" "aZ__0"
      itexpr "Identifier" "a" "a"
      itexpr "Identifier" "Af" "Af"

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



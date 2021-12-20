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
  describe "#PrimaryExpression" $ do
    describe "Identifier" $ do
      it "should transpile Solidity `Identifier` correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "aZ_$0"
        scryptCode tr `shouldBe` "aZ__0"

    describe "#BooleanLiteral" $ do
      it "should transpile Solidity `BooleanLiteral` correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "true"
        scryptCode tr `shouldBe` "true"
        tr1 :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "false"
        scryptCode tr1 `shouldBe` "false"

    describe "#NumberLiteral" $ do
      it "should transpile Solidity `NumberLiteralHex` correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "0x123a"
        scryptCode tr `shouldBe` "0x123a"
      
      it "should transpile Solidity `NumberLiteralDec` non-negative Integer correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "255"
        scryptCode tr `shouldBe` "255"

      it "should transpile Solidity `NumberLiteralDec` negative Integer  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "-255"
        scryptCode tr `shouldBe` "-255"

    describe "#HexLiteral" $ do
      it "should transpile Solidity `HexLiteral` correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "hex\"010113\""
        scryptCode tr `shouldBe` "b'010113'"
      
      it "should transpile Solidity `HexLiteral` correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "hex\"0aAD\""
        scryptCode tr `shouldBe` "b'0aad'"
      
      it "should transpile Solidity `HexLiteral` empty hex correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "hex\"\""
        scryptCode tr `shouldBe` "b''"



  describe "#Unary Expression" $ do
    describe "#Unary" $ do
      it "should transpile Solidity `-`  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "-(0xf)"
        scryptCode tr `shouldBe` "-(0xf)"

      it "should transpile Solidity `()`  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "(0xf)"
        scryptCode tr `shouldBe` "(0xf)"


  describe "#Binary Expression" $ do
    describe "#Binary" $ do
      it "should transpile Solidity `+`  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "1 + 3"
        scryptCode tr `shouldBe` "1 + 3"

      it "should transpile Solidity `-`  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "1 - 3"
        scryptCode tr `shouldBe` "1 - 3"

      it "should transpile Solidity `*`  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "1 * 3"
        scryptCode tr `shouldBe` "1 * 3"

      it "should transpile Solidity `/`  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "1 / 3"
        scryptCode tr `shouldBe` "1 / 3"

      it "should transpile Solidity `%`  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "1 % 3"
        scryptCode tr `shouldBe` "1 % 3"

      -- it "should transpile Solidity `+=`  correctly" $ do
      -- it "should transpile Solidity `-=`  correctly" $ do
      -- it "should transpile Solidity `*=`  correctly" $ do
      -- it "should transpile Solidity `/=`  correctly" $ do
      -- it "should transpile Solidity `%=`  correctly" $ do


      it "should transpile Solidity `&&`  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "true && false"
        scryptCode tr `shouldBe` "true && false"

      it "should transpile Solidity `||`  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "true || false"
        scryptCode tr `shouldBe` "true || false"

      it "should transpile Solidity `!=`  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "1 != 3"
        scryptCode tr `shouldBe` "1 != 3"

      it "should transpile Solidity `==`  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "1 == 3"
        scryptCode tr `shouldBe` "1 == 3"

      it "should transpile Solidity `<`  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "1 < 3"
        scryptCode tr `shouldBe` "1 < 3"

      it "should transpile Solidity `<=`  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "1 <= 3"
        scryptCode tr `shouldBe` "1 <= 3"


      it "should transpile Solidity `>`  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "1 > 3"
        scryptCode tr `shouldBe` "1 > 3"

      it "should transpile Solidity `>=`  correctly" $ do
        tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "1 >= 3"
        scryptCode tr `shouldBe` "1 >= 3"

 

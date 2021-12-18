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

      -- it "should transpile Solidity `NumberLiteralHex` negative Integer  correctly" $ do
      --   tr :: TranspileResult Expression IExpr' (Maybe (Expr IExpr)) <- transpile "-(0x333)"
      --   scryptCode tr `shouldBe` "-(0x333)"

        

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




            

 
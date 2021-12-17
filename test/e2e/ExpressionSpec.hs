{-# LANGUAGE ScopedTypeVariables #-}

module ExpressionSpec where
import Transpiler
import Solidity as Sol
import Intermediate
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

      it "should transpile Solidity `NumberLiteralDec` correctly" $ do
        tr :: TranspileResult TypeName IType' (Maybe Type) <- transpile "uint256"
        scryptCode tr `shouldBe` "int"

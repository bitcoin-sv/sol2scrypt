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
  describe "#SimpleStatementExpression" $ do
    describe "#NumberLiteral" $ do
      it "should transpile Solidity `NumberLiteralHex` correctly" $ do
        tr :: TranspileResult Sol.Statement IStatement' (Maybe (Scr.Statement Ann)) <- transpile "0x123a;"
        scryptCode tr `shouldBe` "0x123a;"
        tr1 :: TranspileResult Sol.Statement IStatement' (Maybe (Scr.Statement Ann)) <- transpile "256;"
        scryptCode tr1 `shouldBe` "256;"

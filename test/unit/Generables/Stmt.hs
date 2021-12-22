module Generables.Stmt where

import Scrypt.Generables.Base
import Scrypt.Generables.Stmt ()
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance Generable (Stmt a)" $ do
  describe "#ExprStmt" $ do
    it "should generate sCrypt code for `BoolLiteral` correctly" $ do
      genCode (Just $ Scr.ExprStmt (Scr.BoolLiteral True Nothing) Nothing) `shouldBe` "true;"
      genCode (Just $ Scr.ExprStmt (Scr.BoolLiteral False Nothing) Nothing) `shouldBe` "false;"

  it "should generate sCrypt code for `IntLiteral` correctly" $ do
    genCode (Just $ Scr.ExprStmt (Scr.IntLiteral True 15 Nothing) Nothing) `shouldBe` "0xf;"
    genCode (Just $ Scr.ExprStmt (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "15;"

  it "should generate sCrypt code for `HexLiteral` correctly" $ do
    genCode (Just $ Scr.ExprStmt (Scr.BytesLiteral [1,1,19] Nothing) Nothing) `shouldBe` "b'010113';"
    genCode (Just $ Scr.ExprStmt (Scr.BytesLiteral [] Nothing) Nothing) `shouldBe` "b'';"

  it "should generate sCrypt code for `UnaryExpr` correctly" $ do
    genCode (Just $ Scr.ExprStmt (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False Nothing) Nothing) Nothing) `shouldBe` "--a;"

  it "should generate sCrypt code for `BinaryExpr` correctly" $ do
    genCode (Just $ Scr.ExprStmt (Scr.BinaryExpr Scr.Mul (Scr.IntLiteral False 15 Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) Nothing) `shouldBe` "15 * 15;"

  describe "#AssignStmt" $ do
    it "should generate sCrypt code for `BoolLiteral` correctly" $ do
      genCode (Just $ Scr.Assign (Scr.Var "x" False Nothing) (Scr.BoolLiteral True Nothing) Nothing) `shouldBe` "x = true;"

    it "should generate sCrypt code for `IntLiteral` correctly" $ do
      genCode (Just $ Scr.Assign (Scr.Var "x" False Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "x = 15;"

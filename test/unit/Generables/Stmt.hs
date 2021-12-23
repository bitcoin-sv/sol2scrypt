module Generables.Stmt where

import Scrypt.Generables.Base
import Scrypt.Generables.Stmt ()
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance Generable (Stmt a)" $ do
  let itcode title e c = it ("should generate sCrypt code for `" ++ title ++ "` correctly") $ do
        genCode (Just $ Scr.ExprStmt e Nothing) `shouldBe` c

  describe "#Stmt" $ do
    itcode "BoolLiteral" (Scr.BoolLiteral True Nothing) "true;"
    itcode "BoolLiteral" (Scr.BoolLiteral False Nothing) "false;"

    itcode "IntLiteral" (Scr.IntLiteral True 15 Nothing) "0xf;"
    itcode "IntLiteral" (Scr.IntLiteral False 15 Nothing) "15;"

    itcode "HexLiteral" (Scr.BytesLiteral [1, 1, 19] Nothing) "b'010113';"
    itcode "HexLiteral" (Scr.BytesLiteral [] Nothing) "b'';"

    itcode
      "UnaryExpr"
      (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False Nothing) Nothing)
      "--a;"

    itcode
      "BinaryExpr"
      (Scr.BinaryExpr Scr.Mul (Scr.IntLiteral False 15 Nothing) (Scr.IntLiteral False 15 Nothing) Nothing)
      "15 * 15;"

    describe "#AssignStmt" $ do
      it "should generate sCrypt code for `BoolLiteral` correctly" $ do
        genCode (Just $ Scr.Assign (Scr.Var "x" False Nothing) (Scr.BoolLiteral True Nothing) Nothing) `shouldBe` "x = true;"

      it "should generate sCrypt code for `IntLiteral` correctly" $ do
        genCode (Just $ Scr.Assign (Scr.Var "x" False Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "x = 15;"

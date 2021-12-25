module Generables.Stmt where

import Scrypt.Generables.Base
import Scrypt.Generables.Stmt ()
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils

spec :: IO TestTree
spec = testSpec "instance Generable (Stmt a)" $ do
  let itcode title e c = it ("should generate sCrypt code for `" ++ title ++ "` correctly") $ do
        genCode (Just $ Scr.ExprStmt e nil) `shouldBe` c

  describe "#Stmt" $ do
    itcode "BoolLiteral" (Scr.BoolLiteral True nil) "true;"
    itcode "BoolLiteral" (Scr.BoolLiteral False nil) "false;"

    itcode "IntLiteral" (Scr.IntLiteral True 15 nil) "0xf;"
    itcode "IntLiteral" (Scr.IntLiteral False 15 nil) "15;"

    itcode "HexLiteral" (Scr.BytesLiteral [1, 1, 19] nil) "b'010113';"
    itcode "HexLiteral" (Scr.BytesLiteral [] nil) "b'';"

    itcode
      "UnaryExpr"
      (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False nil) nil)
      "--a;"

    itcode
      "BinaryExpr"
      (Scr.BinaryExpr Scr.Mul (Scr.IntLiteral False 15 nil) (Scr.IntLiteral False 15 nil) nil)
      "15 * 15;"

    describe "#AssignStmt" $ do
      let itAssignStmt title e c = it ("should generate sCrypt code for AssignStmt: `" ++ title ++ "` correctly") $ do
            genCode (Just $ Scr.Assign (Scr.Var "x" False nil) e nil) `shouldBe` c

      itAssignStmt "BoolLiteral" (Scr.BoolLiteral True nil) "x = true;"

      itAssignStmt "IntLiteral" (Scr.IntLiteral False 15 nil) "x = 15;"

      itAssignStmt "IntLiteral" (Scr.IntLiteral True 15 nil) "x = 0xf;"

      itAssignStmt "BytesLiteral" (Scr.BytesLiteral [1, 1, 19] nil) "x = b'010113';"

  describe "#DeclareStmt" $ do
    let param typ = Scr.Param (TypeAnn typ nil) (NameAnn "x" nil) (Const False) Nothing Public (IsStateProp False) nil
    it "should generate sCrypt code for `BoolLiteral` correctly" $ do
      genCode (Just $ Scr.Declare (param Scr.Bool) (Scr.BoolLiteral True nil) nil) `shouldBe` "bool x = true;"

    it "should generate sCrypt code for `IntLiteral` correctly" $ do
      genCode (Just $ Scr.Declare (param Scr.Int) (Scr.IntLiteral False 15 nil) nil) `shouldBe` "int x = 15;"

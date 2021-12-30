module Generables.Statement where

import Scrypt.Generables.Base
import Scrypt.Generables.Statement ()
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils

spec :: IO TestTree
spec = testSpec "instance Generable (Stmt a)" $ do
  let itcode title e c = it ("should generate sCrypt code for `" ++ title ++ "` correctly") $ do
        genCode e `shouldBe` c

  describe "#Stmt" $ do
    itcode "BoolLiteral" (Scr.ExprStmt (Scr.BoolLiteral True nil) nil) "true;"
    itcode "BoolLiteral" (Scr.ExprStmt (Scr.BoolLiteral False nil) nil) "false;"

    itcode "IntLiteral" (Scr.ExprStmt (Scr.IntLiteral True 15 nil) nil) "0xf;"
    itcode "IntLiteral" (Scr.ExprStmt (Scr.IntLiteral False 15 nil) nil) "15;"

    itcode "HexLiteral" (Scr.ExprStmt (Scr.BytesLiteral [1, 1, 19] nil) nil) "b'010113';"
    itcode "HexLiteral" (Scr.ExprStmt (Scr.BytesLiteral [] nil) nil) "b'';"

    itcode
      "UnaryExpr"
      (Scr.ExprStmt (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False nil) nil) nil)
      "--a;"

    itcode
      "BinaryExpr"
      (Scr.ExprStmt (Scr.BinaryExpr Scr.Mul (Scr.IntLiteral False 15 nil) (Scr.IntLiteral False 15 nil) nil) nil)
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

    itcode "BoolLiteral" (Scr.Declare (param Scr.Bool) (Scr.BoolLiteral True nil) nil) "bool x = true;"

    itcode "IntLiteral" (Scr.Declare (param Scr.Int) (Scr.IntLiteral False 15 nil) nil) "int x = 15;"

    itcode "BytesLiteral" (Scr.Declare (param Scr.Bytes) (Scr.BytesLiteral [1, 1, 19] nil) nil) "bytes x = b'010113';"

    itcode "UnaryExpr" (Scr.Declare (param Scr.Int) (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False nil) nil) nil) "int x = --a;"

    itcode "BinaryExpr" (Scr.Declare (param Scr.Int) (Scr.BinaryExpr Scr.Mul (Scr.IntLiteral False 15 nil) (Scr.IntLiteral False 15 nil) nil) nil) "int x = 15 * 15;"

  describe "#BlockStmt" $ do
    let param typ = Scr.Param (TypeAnn typ nil) (NameAnn "x" nil) (Const False) Nothing Public (IsStateProp False) nil
        declare typ = Scr.Declare (param typ) (Scr.BoolLiteral True nil) nil
        assign e = Scr.Assign (Scr.Var "x" False nil) e nil
        unary = Scr.ExprStmt (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False nil) nil) nil
   
    itcode "Declare" (Scr.Block [declare Scr.Bool] nil) "{bool x = true;}"
    itcode "assign" (Scr.Block [assign (Scr.IntLiteral False 15 nil) ] nil) "{x = 15;}"
    itcode "assign" (Scr.Block [assign (Scr.IntLiteral False 15 nil) , unary, declare Scr.Bool] nil) "{x = 15; --a; bool x = true;}"

  describe "#ReturnStmt" $ do
    itcode "BoolLiteral" (Scr.ReturnStmt (Scr.BoolLiteral True nil) nil) "return true;"
    itcode "BoolLiteral" (Scr.ReturnStmt (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False nil) nil) nil) "return --a;"
    itcode "BinaryExpr" (Scr.ReturnStmt (Scr.BinaryExpr Scr.Mul (Scr.IntLiteral False 15 nil) (Scr.IntLiteral False 15 nil) nil) nil) "return 15 * 15;"

  describe "#RequireStmt" $ do
    itcode "BoolLiteral" (Scr.Require (Scr.BoolLiteral True nil) nil) "require(true);"
    itcode "BoolLiteral" (Scr.Require (Scr.UnaryExpr Scr.Not (Scr.Var "a" False nil) nil) nil) "require(!a);"
    itcode "BinaryExpr" (Scr.Require (Scr.BinaryExpr Scr.Equal (Scr.IntLiteral False 15 nil) (Scr.IntLiteral False 15 nil) nil) nil) "require(15 == 15);"

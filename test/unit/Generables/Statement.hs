{-# LANGUAGE QuasiQuotes #-}

module Generables.Statement where

import Scrypt.Generables.Base
import Scrypt.Generables.Statement ()
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Text.RawString.QQ
import Utils

spec :: IO TestTree
spec = testSpec "instance Generable (Stmt a)" $ do
  let itcode title e c = it ("should generate sCrypt code for `" ++ title ++ "` correctly") $ do
        e' <- generateScrypt (CodeGenState 0) e
        e' `shouldBe` c

  describe "#Stmt" $ do
    itcode "BoolLiteral" (Scr.ExprStmt (Scr.BoolLiteral True nil) nil) "\ntrue;"
    itcode "BoolLiteral" (Scr.ExprStmt (Scr.BoolLiteral False nil) nil) "\nfalse;"

    itcode "IntLiteral" (Scr.ExprStmt (Scr.IntLiteral True 15 nil) nil) "\n0xf;"
    itcode "IntLiteral" (Scr.ExprStmt (Scr.IntLiteral False 15 nil) nil) "\n15;"

    itcode "HexLiteral" (Scr.ExprStmt (Scr.BytesLiteral False [1, 1, 19] nil) nil) "\nb'010113';"
    itcode "HexLiteral" (Scr.ExprStmt (Scr.BytesLiteral False [] nil) nil) "\nb'';"

    itcode
      "UnaryExpr"
      (Scr.ExprStmt (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False nil) nil) nil)
      "\n--a;"

    itcode
      "BinaryExpr"
      (Scr.ExprStmt (Scr.BinaryExpr Scr.Mul (Scr.IntLiteral False 15 nil) (Scr.IntLiteral False 15 nil) nil) nil)
      "\n15 * 15;"

    describe "#AssignStmt" $ do
      let itAssignStmt title e c = it ("should generate sCrypt code for AssignStmt: `" ++ title ++ "` correctly") $ do
            r <- generateScrypt (CodeGenState 0) (Just $ Scr.Assign (Scr.Var "x" False nil) e nil) 
            r `shouldBe` c

      itAssignStmt "BoolLiteral" (Scr.BoolLiteral True nil) "\nx = true;"

      itAssignStmt "IntLiteral" (Scr.IntLiteral False 15 nil) "\nx = 15;"

      itAssignStmt "IntLiteral" (Scr.IntLiteral True 15 nil) "\nx = 0xf;"

      itAssignStmt "BytesLiteral" (Scr.BytesLiteral False [1, 1, 19] nil) "\nx = b'010113';"

  describe "#DeclareStmt" $ do
    let param typ = Scr.Param (TypeAnn typ nil) (NameAnn "x" nil) (Const False) Nothing Public (IsStateProp False) nil

    itcode "BoolLiteral" (Scr.Declare (param Scr.Bool) (Scr.BoolLiteral True nil) nil) "\nbool x = true;"

    itcode "IntLiteral" (Scr.Declare (param Scr.Int) (Scr.IntLiteral False 15 nil) nil) "\nint x = 15;"

    itcode "BytesLiteral" (Scr.Declare (param Scr.Bytes) (Scr.BytesLiteral False [1, 1, 19] nil) nil) "\nbytes x = b'010113';"

    itcode "UnaryExpr" (Scr.Declare (param Scr.Int) (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False nil) nil) nil) "\nint x = --a;"

    itcode "BinaryExpr" (Scr.Declare (param Scr.Int) (Scr.BinaryExpr Scr.Mul (Scr.IntLiteral False 15 nil) (Scr.IntLiteral False 15 nil) nil) nil) "\nint x = 15 * 15;"

  describe "#BlockStmt" $ do
    let param typ = Scr.Param (TypeAnn typ nil) (NameAnn "x" nil) (Const False) Nothing Public (IsStateProp False) nil
        declare typ = Scr.Declare (param typ) (Scr.BoolLiteral True nil) nil
        assign e = Scr.Assign (Scr.Var "x" False nil) e nil
        unary = Scr.ExprStmt (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False nil) nil) nil
   
    itcode "Declare" (Scr.Block [declare Scr.Bool] nil) "\n{\n  bool x = true;\n}"
    itcode "assign" (Scr.Block [assign (Scr.IntLiteral False 15 nil) ] nil) "\n{\n  x = 15;\n}"
    itcode "assign" (Scr.Block [assign (Scr.IntLiteral False 15 nil) , unary, declare Scr.Bool] nil)
      [r|
{
  x = 15;
  --a;
  bool x = true;
}|]

  describe "#ReturnStmt" $ do
    itcode "BoolLiteral" (Scr.ReturnStmt (Scr.BoolLiteral True nil) nil) "\nreturn true;"
    itcode "BoolLiteral" (Scr.ReturnStmt (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False nil) nil) nil) "\nreturn --a;"
    itcode "BinaryExpr" (Scr.ReturnStmt (Scr.BinaryExpr Scr.Mul (Scr.IntLiteral False 15 nil) (Scr.IntLiteral False 15 nil) nil) nil) "\nreturn 15 * 15;"

  describe "#RequireStmt" $ do
    itcode "BoolLiteral" (Scr.Require (Scr.BoolLiteral True nil) nil) "\nrequire(true);"
    itcode "BoolLiteral" (Scr.Require (Scr.UnaryExpr Scr.Not (Scr.Var "a" False nil) nil) nil) "\nrequire(!a);"
    itcode "BinaryExpr" (Scr.Require (Scr.BinaryExpr Scr.Equal (Scr.IntLiteral False 15 nil) (Scr.IntLiteral False 15 nil) nil) nil) "\nrequire(15 == 15);"

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
      let itAssignStmt title e c = it ("should generate sCrypt code for AssignStmt: `" ++ title ++ "` correctly") $ do
            genCode (Just $ Scr.Assign (Scr.Var "x" False Nothing) e Nothing) `shouldBe` c

      itAssignStmt "BoolLiteral" (Scr.BoolLiteral True Nothing) "x = true;"

      itAssignStmt "IntLiteral" (Scr.IntLiteral False 15 Nothing) "x = 15;"

      itAssignStmt "IntLiteral" (Scr.IntLiteral True 15 Nothing) "x = 0xf;"

      itAssignStmt "BytesLiteral" (Scr.BytesLiteral [1, 1, 19] Nothing) "x = b'010113';"
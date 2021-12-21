module Generables.Stmt where

import Scrypt.Generables.Base
import Scrypt.Generables.Stmt ()
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance Generable (Stmt a)" $ do
  it "should generate sCrypt code for `BoolLiteral` correctly" $ do
    genCode (Just $ Scr.ExprStmt (Scr.BoolLiteral True Nothing) Nothing) `shouldBe` "true;"
    genCode (Just $ Scr.ExprStmt (Scr.BoolLiteral False Nothing) Nothing) `shouldBe` "false;"

  it "should generate sCrypt code for `IntLiteral` correctly" $ do
    genCode (Just $ Scr.ExprStmt (Scr.IntLiteral True 15 Nothing) Nothing) `shouldBe` "0xf;"
    genCode (Just $ Scr.ExprStmt (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "15;"

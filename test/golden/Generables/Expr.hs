module Generables.Expr where

import Scrypt.Generables.Base
import Scrypt.Generables.Expr ()
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance Generable (Expr a)" $ do
  it "should generate sCrypt code for `BoolLiteral` correctly" $ do
    genCode (Just $ Scr.BoolLiteral True Nothing) `shouldBe` "true"
    genCode (Just $ Scr.BoolLiteral False Nothing) `shouldBe` "false"

  it "should generate sCrypt code for `IntLiteral` correctly" $ do
    genCode (Just $ Scr.IntLiteral True 15 Nothing) `shouldBe` "0x0f"
    genCode (Just $ Scr.IntLiteral False 15 Nothing) `shouldBe` "15"

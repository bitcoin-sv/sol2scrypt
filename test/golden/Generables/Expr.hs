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
    genCode (Just $ Scr.IntLiteral True 15 Nothing) `shouldBe` "0xf"
    genCode (Just $ Scr.IntLiteral False 15 Nothing) `shouldBe` "15"
    genCode (Just $ Scr.IntLiteral True 32 Nothing) `shouldBe` "0x20"

  it "should generate sCrypt code for `BytesLiteral` correctly" $ do
    genCode (Just $ Scr.BytesLiteral [1,1,19] Nothing) `shouldBe` "b'010113'"

  it "should generate sCrypt code for `Parens` correctly"  $ do
    genCode (Just $ Scr.Parens (Scr.IntLiteral False 0 Nothing) Nothing) `shouldBe` "(0)"

  it "should generate sCrypt code for `UnaryExpr Negate` correctly"  $ do
    genCode (Just $ Scr.UnaryExpr Negate (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "-15"
    genCode (Just $ Scr.UnaryExpr Negate (Scr.IntLiteral True 15 Nothing) Nothing) `shouldBe` "-0xf"
    genCode (Just $ Scr.UnaryExpr Negate (Scr.Parens (Scr.IntLiteral True 15 Nothing) Nothing) Nothing) `shouldBe` "-(0xf)"


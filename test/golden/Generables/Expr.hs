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
    genCode (Just $ Scr.IntLiteral False (-15) Nothing) `shouldBe` "-15"
    genCode (Just $ Scr.IntLiteral True (-15) Nothing) `shouldBe` "-(0xf)"
    genCode (Just $ Scr.IntLiteral True (-1) Nothing) `shouldBe` "-(0x1)"
    genCode (Just $ Scr.IntLiteral False (-1000000000000000000000) Nothing) `shouldBe` "-1000000000000000000000"
    genCode (Just $ Scr.IntLiteral True (-1000000000000000000000) Nothing) `shouldBe` "-(0x3635c9adc5dea00000)"

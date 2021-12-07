module Generables.Type where

import Scrypt.Generables.Base
import Scrypt.Generables.Type ()
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance Generable Scr.Type" $ do
  it "should generate sCrypt code for `Bool` correctly" $ do
    genCode (Just Scr.Bool) `shouldBe` "bool"

  it "should generate sCrypt code for `Int` correctly" $ do
    genCode (Just Scr.Int) `shouldBe` "int"

module Transformations.IntM2Scr.Expression where

import Intermediate.Spec as IntM
import Intermediate.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "Transformations.IntM2Scr.Expression" $ do
  describe "#transformIntermediateExpr" $ do
    it "should transform Intermediate `BoolLiteral` to sCrypt Type correctly" $ do
      let e1 = IntM.BoolLiteral True
      transformIntermediateExpr e1 `shouldBe` Scr.BoolLiteral True e1
      let e2 = IntM.BoolLiteral False
      transformIntermediateExpr e2 `shouldBe` Scr.BoolLiteral False e2

    it "should transform Intermediate `IntLiteral` to sCrypt Type correctly" $ do
      let e1 = IntM.IntLiteral True 15
      transformIntermediateExpr e1 `shouldBe` Scr.IntLiteral True 15 e1
      let e2 = IntM.IntLiteral False 15
      transformIntermediateExpr e2 `shouldBe` Scr.IntLiteral False 15 e2

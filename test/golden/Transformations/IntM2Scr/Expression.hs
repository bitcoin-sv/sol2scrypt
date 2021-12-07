module Transformations.IntM2Scr.Expression where

import Intermediate.Spec as IntM
import Intermediate.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToScryptTransformable IExpr (Scr.Expr IExpr)" $ do
  it "should transform Intermediate `BoolLiteral` to sCrypt Type correctly" $ do
    let e1 = IntM.BoolLiteral True
    r1 <- transform2Scrypt $ Just e1 
    r1 `shouldBe` Just (Scr.BoolLiteral True e1)
    let e2 = IntM.BoolLiteral False
    r2 <- transform2Scrypt $ Just e2 
    r2 `shouldBe` Just (Scr.BoolLiteral False e2)

  it "should transform Intermediate `IntLiteral` to sCrypt Type correctly" $ do
    let e1 = IntM.IntLiteral True 15
    r1 <- transform2Scrypt $ Just e1 
    r1 `shouldBe` Just (Scr.IntLiteral True 15 e1)
    let e2 = IntM.IntLiteral False 15
    r2 <- transform2Scrypt $ Just e2 
    r2 `shouldBe` Just (Scr.IntLiteral False 15 e2)

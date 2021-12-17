module Transformations.IR2Scr.Expression where

import IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToScryptTransformable IExpr (Scr.Expr IExpr)" $ do
  it "should transform IR `BoolLiteral` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.BoolLiteral True
    r1 <- transform2Scrypt $ Just e1 
    r1 `shouldBe` Just (Scr.BoolLiteral True e1)
    let e2 = LiteralExpr $ IR.BoolLiteral False
    r2 <- transform2Scrypt $ Just e2 
    r2 `shouldBe` Just (Scr.BoolLiteral False e2)

  it "should transform IR `IntLiteral` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    r1 <- transform2Scrypt $ Just e1 
    r1 `shouldBe` Just (Scr.IntLiteral True 15 e1)
    let e2 = LiteralExpr $ IR.IntLiteral False 15
    r2 <- transform2Scrypt $ Just e2 
    r2 `shouldBe` Just (Scr.IntLiteral False 15 e2)
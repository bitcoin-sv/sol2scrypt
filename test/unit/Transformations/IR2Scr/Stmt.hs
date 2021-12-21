module Transformations.IR2Scr.Stmt where

import IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToScryptTransformable IStatment (Scr.Statement IExpr)" $ do
  it "should transform IR `BoolLiteral` to sCrypt Statment correctly" $ do
    let e1 = LiteralExpr $ IR.BoolLiteral True
    r1 <- transform2Scrypt $ Just . IR.ExprStmt $ e1 
    r1 `shouldBe` Just (Scr.ExprStmt (Scr.BoolLiteral True e1) e1)
    let e2 = LiteralExpr $ IR.BoolLiteral False
    r2 <- transform2Scrypt $ Just . IR.ExprStmt $ e2 
    r2 `shouldBe` Just (Scr.ExprStmt (Scr.BoolLiteral False e2) e2)

  it "should transform IR `IntLiteral` to sCrypt Statment correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    r1 <- transform2Scrypt $ Just . IR.ExprStmt $ e1 
    r1 `shouldBe` Just (Scr.ExprStmt (Scr.IntLiteral True 15 e1) e1)
    let e2 = LiteralExpr $ IR.IntLiteral False 15
    r2 <- transform2Scrypt $ Just . IR.ExprStmt $ e2 
    r2 `shouldBe` Just (Scr.ExprStmt (Scr.IntLiteral False 15 e2) e2)

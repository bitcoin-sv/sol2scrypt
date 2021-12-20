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

  it "should transform IR `BytesLiteral` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.BytesLiteral [1,1,19]
    r1 <- transform2Scrypt $ Just e1 
    r1 `shouldBe` Just (Scr.BytesLiteral [1,1,19] e1)


  it "should transform IR `Parens` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = IR.Parens e1
    r1 <- transform2Scrypt $ Just e2
    r1 `shouldBe` Just (Scr.Parens (Scr.IntLiteral True 15 e1) e2)

  it "should transform IR `UnaryExpr Negate` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = IR.UnaryExpr IR.Negate e1
    r1 <- transform2Scrypt $ Just e2
    r1 `shouldBe` Just (Scr.UnaryExpr Scr.Negate (Scr.IntLiteral True 15 e1) e2)

  it "should transform IR `BinaryExpr +` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.Add e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.Add (Scr.IntLiteral True 15 e1) (Scr.IntLiteral True 15 e2) e)

  it "should transform IR `BinaryExpr -` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.Sub  e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.Sub (Scr.IntLiteral True 15 e1) (Scr.IntLiteral True 15 e2) e)

  it "should transform IR `BinaryExpr *` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.Mul  e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.Mul (Scr.IntLiteral True 15 e1) (Scr.IntLiteral True 15 e2) e)


  it "should transform IR `BinaryExpr /` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.Div  e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.Div (Scr.IntLiteral True 15 e1) (Scr.IntLiteral True 15 e2) e)

  it "should transform IR `BinaryExpr %` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.Mod e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.Mod (Scr.IntLiteral True 15 e1) (Scr.IntLiteral True 15 e2) e)

  -- it "should transform IR `BinaryExpr +=` to sCrypt Type correctly" $ do

  -- it "should transform IR `BinaryExpr -=` to sCrypt Type correctly" $ do

  -- it "should transform IR `BinaryExpr *=` to sCrypt Type correctly" $ do

  -- it "should transform IR `BinaryExpr /=` to sCrypt Type correctly" $ do

  -- it "should transform IR `BinaryExpr %=` to sCrypt Type correctly" $ do

  it "should transform IR `BinaryExpr ==` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.Equal e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.Equal (Scr.IntLiteral True 15 e1) (Scr.IntLiteral True 15 e2) e)

  it "should transform IR `BinaryExpr !=` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 3
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.Neq e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.Neq (Scr.IntLiteral True 3 e1) (Scr.IntLiteral True 15 e2) e)

  it "should transform IR `BinaryExpr &&` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.BoolLiteral True
    let e2 = LiteralExpr $ IR.BoolLiteral False
    let e = IR.BinaryExpr IR.BoolAnd e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.BoolAnd (Scr.BoolLiteral True e1) (Scr.BoolLiteral False e2) e)

  it "should transform IR `BinaryExpr ||` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.BoolLiteral True
    let e2 = LiteralExpr $ IR.BoolLiteral False
    let e = IR.BinaryExpr IR.BoolOr e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.BoolOr (Scr.BoolLiteral True e1) (Scr.BoolLiteral False e2) e)


  it "should transform IR `BinaryExpr <` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 3
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.LessThan e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.LessThan (Scr.IntLiteral True 3 e1) (Scr.IntLiteral True 15 e2) e)


  it "should transform IR `BinaryExpr <=` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 3
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.LessThanOrEqual e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.LessThanOrEqual (Scr.IntLiteral True 3 e1) (Scr.IntLiteral True 15 e2) e)

  it "should transform IR `BinaryExpr >` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 3
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.GreaterThan e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.GreaterThan (Scr.IntLiteral True 3 e1) (Scr.IntLiteral True 15 e2) e)

  it "should transform IR `BinaryExpr >=` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 3
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.GreaterThanOrEqual e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.GreaterThanOrEqual (Scr.IntLiteral True 3 e1) (Scr.IntLiteral True 15 e2) e)

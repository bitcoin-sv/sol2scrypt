module Transformations.IR2Scr.Expression where

import IR.Spec as IR
import IR.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec
import Utils

spec :: IO TestTree
spec = testSpec "instance ToScryptTransformable IExpr (Scr.Expr Ann)" $ do
  it "should transform IR `BoolLiteral` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.BoolLiteral True
    r1 <- transform2Scrypt $ Just e1 
    r1 `shouldBe` Just (Scr.BoolLiteral True nil)
    let e2 = LiteralExpr $ IR.BoolLiteral False
    r2 <- transform2Scrypt $ Just e2 
    r2 `shouldBe` Just (Scr.BoolLiteral False nil)

  it "should transform IR `IntLiteral` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    r1 <- transform2Scrypt $ Just e1 
    r1 `shouldBe` Just (Scr.IntLiteral True 15 nil)
    let e2 = LiteralExpr $ IR.IntLiteral False 15
    r2 <- transform2Scrypt $ Just e2 
    r2 `shouldBe` Just (Scr.IntLiteral False 15 nil)

  it "should transform IR `BytesLiteral` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.BytesLiteral [1,1,19]
    r1 <- transform2Scrypt $ Just e1 
    r1 `shouldBe` Just (Scr.BytesLiteral [1,1,19] nil)


  it "should transform IR `Parens` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = IR.Parens e1
    r1 <- transform2Scrypt $ Just e2
    r1 `shouldBe` Just (Scr.Parens (Scr.IntLiteral True 15 nil) nil)

  it "should transform IR `UnaryExpr Negate` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = IR.UnaryExpr IR.Negate e1
    r1 <- transform2Scrypt $ Just e2
    r1 `shouldBe` Just (Scr.UnaryExpr Scr.Negate (Scr.IntLiteral True 15 nil) nil)

  it "should transform IR `UnaryExpr ++` to sCrypt Type correctly" $ do
    let e1 = IR.IdentifierExpr $ IR.IIdentifier "a"
    let e2 = IR.UnaryExpr IR.PreIncrement e1
    r1 <- transform2Scrypt $ Just e2
    r1 `shouldBe` Just (Scr.UnaryExpr Scr.PreIncrement (Scr.Var "a" False e1) e2)

  it "should transform IR `UnaryExpr ()++` to sCrypt Type correctly" $ do
    let e1 = IR.IdentifierExpr $ IR.IIdentifier "a"
    let e2 = IR.UnaryExpr IR.PostIncrement e1
    r1 <- transform2Scrypt $ Just e2
    r1 `shouldBe` Just (Scr.UnaryExpr Scr.PostIncrement (Scr.Var "a" False e1) e2)

  it "should transform IR `UnaryExpr --` to sCrypt Type correctly" $ do
    let e1 = IR.IdentifierExpr $ IR.IIdentifier "a"
    let e2 = IR.UnaryExpr IR.PreDecrement e1
    r1 <- transform2Scrypt $ Just e2
    r1 `shouldBe` Just (Scr.UnaryExpr Scr.PreDecrement (Scr.Var "a" False e1) e2)

  it "should transform IR `UnaryExpr ()--` to sCrypt Type correctly" $ do
    let e1 = IR.IdentifierExpr $ IR.IIdentifier "a"
    let e2 = IR.UnaryExpr IR.PostDecrement e1
    r1 <- transform2Scrypt $ Just e2
    r1 `shouldBe` Just (Scr.UnaryExpr Scr.PostDecrement (Scr.Var "a" False e1) e2)

  it "should transform IR `UnaryExpr !` to sCrypt Type correctly" $ do
    let e1 = IR.IdentifierExpr $ IR.IIdentifier "a"
    let e2 = IR.UnaryExpr IR.Not e1
    r1 <- transform2Scrypt $ Just e2
    r1 `shouldBe` Just (Scr.UnaryExpr Scr.Not (Scr.Var "a" False e1) e2)

  it "should transform IR `BinaryExpr +` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.Add e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.Add (Scr.IntLiteral True 15 nil) (Scr.IntLiteral True 15 nil) nil)

  it "should transform IR `BinaryExpr -` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.Sub  e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.Sub (Scr.IntLiteral True 15 nil) (Scr.IntLiteral True 15 nil) nil)

  it "should transform IR `BinaryExpr *` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.Mul  e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.Mul (Scr.IntLiteral True 15 nil) (Scr.IntLiteral True 15 nil) nil)


  it "should transform IR `BinaryExpr /` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.Div  e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.Div (Scr.IntLiteral True 15 nil) (Scr.IntLiteral True 15 nil) nil)

  it "should transform IR `BinaryExpr %` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.Mod e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.Mod (Scr.IntLiteral True 15 nil) (Scr.IntLiteral True 15 nil) nil)

  it "should transform IR `BinaryExpr +=` to sCrypt Type correctly" $ do
    let e1 = IdentifierExpr $ IR.Identifier "a"
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.AddAssign e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.AddAssign (Scr.Var "a" False nil) (Scr.IntLiteral True 15 nil) nil)

  it "should transform IR `BinaryExpr -=` to sCrypt Type correctly" $ do
    let e1 = IdentifierExpr $ IR.Identifier "a"
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.SubAssign e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.SubAssign (Scr.Var "a" False nil) (Scr.IntLiteral True 15 nil) nil)

  it "should transform IR `BinaryExpr *=` to sCrypt Type correctly" $ do
    let e1 = IdentifierExpr $ IR.Identifier "a"
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.MulAssign e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.MulAssign (Scr.Var "a" False nil) (Scr.IntLiteral True 15 nil) nil)

  it "should transform IR `BinaryExpr /=` to sCrypt Type correctly" $ do
    let e1 = IdentifierExpr $ IR.Identifier "a"
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.MulAssign e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.MulAssign (Scr.Var "a" False nil) (Scr.IntLiteral True 15 nil) nil)

  it "should transform IR `BinaryExpr %=` to sCrypt Type correctly" $ do
    let e1 = IdentifierExpr $ IR.Identifier "a"
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.ModAssign e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.ModAssign (Scr.Var "a" False nil) (Scr.IntLiteral True 15 nil) nil)

  it "should transform IR `BinaryExpr ==` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 15
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.Equal e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.Equal (Scr.IntLiteral True 15 nil) (Scr.IntLiteral True 15 nil) nil)

  it "should transform IR `BinaryExpr !=` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 3
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.Neq e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.Neq (Scr.IntLiteral True 3 nil) (Scr.IntLiteral True 15 nil) nil)

  it "should transform IR `BinaryExpr &&` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.BoolLiteral True
    let e2 = LiteralExpr $ IR.BoolLiteral False
    let e = IR.BinaryExpr IR.BoolAnd e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.BoolAnd (Scr.BoolLiteral True nil) (Scr.BoolLiteral False nil) nil)

  it "should transform IR `BinaryExpr ||` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.BoolLiteral True
    let e2 = LiteralExpr $ IR.BoolLiteral False
    let e = IR.BinaryExpr IR.BoolOr e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.BoolOr (Scr.BoolLiteral True nil) (Scr.BoolLiteral False nil) nil)


  it "should transform IR `BinaryExpr <` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 3
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.LessThan e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.LessThan (Scr.IntLiteral True 3 nil) (Scr.IntLiteral True 15 nil) nil)


  it "should transform IR `BinaryExpr <=` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 3
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.LessThanOrEqual e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.LessThanOrEqual (Scr.IntLiteral True 3 nil) (Scr.IntLiteral True 15 nil) nil)

  it "should transform IR `BinaryExpr >` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 3
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.GreaterThan e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.GreaterThan (Scr.IntLiteral True 3 nil) (Scr.IntLiteral True 15 nil) nil)

  it "should transform IR `BinaryExpr >=` to sCrypt Type correctly" $ do
    let e1 = LiteralExpr $ IR.IntLiteral True 3
    let e2 = LiteralExpr $ IR.IntLiteral True 15
    let e = IR.BinaryExpr IR.GreaterThanOrEqual e1 e2
    r1 <- transform2Scrypt $ Just e
    r1 `shouldBe` Just (Scr.BinaryExpr Scr.GreaterThanOrEqual (Scr.IntLiteral True 3 nil) (Scr.IntLiteral True 15 nil) nil)

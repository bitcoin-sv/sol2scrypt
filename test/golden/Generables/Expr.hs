module Generables.Expr where

import Scrypt.Generables.Base
import Scrypt.Generables.Expr ()
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance Generable (Expr a)" $ do

  describe "#Expr" $ do
    describe "#Literal" $ do

      it "should generate sCrypt code for `BoolLiteral` correctly" $ do
        genCode (Just $ Scr.BoolLiteral True Nothing) `shouldBe` "true"
        genCode (Just $ Scr.BoolLiteral False Nothing) `shouldBe` "false"

      it "should generate sCrypt code for `IntLiteral` correctly" $ do
        genCode (Just $ Scr.IntLiteral True 15 Nothing) `shouldBe` "0xf"
        genCode (Just $ Scr.IntLiteral False 15 Nothing) `shouldBe` "15"
        genCode (Just $ Scr.IntLiteral True 32 Nothing) `shouldBe` "0x20"

      it "should generate sCrypt code for `BytesLiteral` correctly" $ do
        genCode (Just $ Scr.BytesLiteral [1,1,19] Nothing) `shouldBe` "b'010113'"

    describe "#Parens" $ do

      it "should generate sCrypt code for `Parens` correctly"  $ do
        genCode (Just $ Scr.Parens (Scr.IntLiteral False 0 Nothing) Nothing) `shouldBe` "(0)"

    describe "#UnaryExpr" $ do

      it "should generate sCrypt code for `UnaryExpr -` correctly"  $ do
        genCode (Just $ Scr.UnaryExpr Negate (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "-15"
        genCode (Just $ Scr.UnaryExpr Negate (Scr.IntLiteral True 15 Nothing) Nothing) `shouldBe` "-0xf"
        genCode (Just $ Scr.UnaryExpr Negate (Scr.Parens (Scr.IntLiteral True 15 Nothing) Nothing) Nothing) `shouldBe` "-(0xf)"

    describe "#BinaryExpr" $ do

      it "should generate sCrypt code for `BinaryExpr +` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.Add (Scr.IntLiteral False 15 Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "15 + 15"

      it "should generate sCrypt code for `BinaryExpr -` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.Sub (Scr.IntLiteral False 15 Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "15 - 15"

      it "should generate sCrypt code for `BinaryExpr *` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.Mul (Scr.IntLiteral False 15 Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "15 * 15"

      it "should generate sCrypt code for `BinaryExpr /` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.Div (Scr.IntLiteral False 15 Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "15 / 15"

      it "should generate sCrypt code for `BinaryExpr %` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.Mod (Scr.IntLiteral False 15 Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "15 % 15"

      it "should generate sCrypt code for `BinaryExpr +=` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.AddAssign (Scr.Var "a" False Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "a += 15"

      it "should generate sCrypt code for `BinaryExpr -=` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.SubAssign (Scr.Var "a" False Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "a -= 15"

      it "should generate sCrypt code for `BinaryExpr *=` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.MulAssign (Scr.Var "a" False Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "a *= 15"

      it "should generate sCrypt code for `BinaryExpr /=` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.DivAssign (Scr.Var "a" False Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "a /= 15"

      it "should generate sCrypt code for `BinaryExpr %=` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.ModAssign (Scr.Var "a" False Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "a %= 15"

      it "should generate sCrypt code for `BinaryExpr ==` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.Equal (Scr.IntLiteral False 15 Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "15 == 15"

      it "should generate sCrypt code for `BinaryExpr !=` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.Neq (Scr.IntLiteral False 15 Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "15 != 15"

      it "should generate sCrypt code for `BinaryExpr ||` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.BoolOr (Scr.BoolLiteral True Nothing) (Scr.BoolLiteral False Nothing) Nothing) `shouldBe` "true || false"

      it "should generate sCrypt code for `BinaryExpr &&` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.BoolAnd (Scr.BoolLiteral True Nothing) (Scr.BoolLiteral False Nothing) Nothing) `shouldBe` "true && false"

      it "should generate sCrypt code for `BinaryExpr <` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.LessThan (Scr.IntLiteral False 1 Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "1 < 15"

      it "should generate sCrypt code for `BinaryExpr <=` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.LessThanOrEqual (Scr.IntLiteral False 1 Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "1 <= 15"

      it "should generate sCrypt code for `BinaryExpr >` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.GreaterThan (Scr.IntLiteral False 1 Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "1 > 15"

      it "should generate sCrypt code for `BinaryExpr >=` correctly"  $ do
        genCode (Just $ Scr.BinaryExpr Scr.GreaterThanOrEqual (Scr.IntLiteral False 1 Nothing) (Scr.IntLiteral False 15 Nothing) Nothing) `shouldBe` "1 >= 15"


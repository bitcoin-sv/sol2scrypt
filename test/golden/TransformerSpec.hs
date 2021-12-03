module TransformerSpec (transformerTests) where

import Intermediate.Spec as IntM
import Intermediate.Transformer
import Scrypt.Spec as Scr
import Test.Tasty
import Test.Tasty.Hspec

transformerTests :: IO TestTree
transformerTests = testSpec "Transformations" spec

spec :: Spec
spec = do
  describe "Solidity to Intermidate" $ do
    describe "#transformSolTypeName" $ do
      it "should transfrom Solidity `BoolType` to Intermediate Type correctly" $ do
        r <- transformSolTypeName "bool"
        r `shouldBe` Just ITypeBool

      it "should transfrom Solidity `IntType` to Intermediate Type correctly" $ do
        r1 <- transformSolTypeName "int"
        r1 `shouldBe` Just ITypeInt
        r2 <- transformSolTypeName "int256"
        r2 `shouldBe` Just ITypeInt

    describe "#transformSolExpression" $ do
      it "should transfrom Solidity `BoolLiteral` to Intermediate Type correctly" $ do
        r1 <- transformSolExpression "true"
        r1 `shouldBe` Just (IntM.BoolLiteral True)
        r2 <- transformSolExpression "false"
        r2 `shouldBe` Just (IntM.BoolLiteral False)

      it "should transfrom Solidity `NumberLiteral` to Intermediate Type correctly" $ do
        r1 <- transformSolExpression "0x0123abcdef"
        r1 `shouldBe` Just (IntM.IntLiteral True 4893429231)
        r2 <- transformSolExpression "12345"
        r2 `shouldBe` Just (IntM.IntLiteral False 12345)

  describe "Intermidate to sCrypt" $ do
    describe "#transformIntermediateType" $ do
      it "should transform Intermediate `ITypeBool` to sCrypt Type correctly" $ do
        transformIntermediateType ITypeBool `shouldBe` Scr.Bool

      it "should transform Intermediate `ITypeInt` to sCrypt Type correctly" $ do
        transformIntermediateType ITypeInt `shouldBe` Scr.Int

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
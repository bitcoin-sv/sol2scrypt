module GeneratorSpec (generatorTests) where

import Intermediate.Spec as IntM
import Scrypt.Spec as Scr
import Scrypt.Generator
import Test.Tasty
import Test.Tasty.Hspec

generatorTests :: IO TestTree
generatorTests = testSpec "Generator" spec

spec :: Spec
spec = do
  describe "#generateScryptFromIType" $ do
    context "when Nothing is given" $
      it "should return empty code" $ do
        r <- generateScryptFromIType Nothing
        r `shouldBe` ""

    context "when Intermediate Type is given" $ do
      it "should generate sCrypt code for `ITypeBool` correctly" $ do
        r <- generateScryptFromIType $ Just ITypeBool
        r `shouldBe` "bool"
      
      it "should generate sCrypt code for `ITypeInt` correctly" $ do
        r <- generateScryptFromIType $ Just ITypeInt
        r `shouldBe` "int"

  describe "#generateScryptFromIExpr" $ do
    context "when Nothing is given" $
      it "should return empty code" $ do
        r <- generateScryptFromIType Nothing
        r `shouldBe` ""
    
    context "when Intermediate Expr is given" $ do
      it "should generate sCrypt code for `BoolLiteral` correctly" $ do
        r1 <- generateScryptFromIExpr $ Just $ IntM.BoolLiteral True
        r1 `shouldBe` "true"
        r2 <- generateScryptFromIExpr $ Just $ IntM.BoolLiteral False
        r2 `shouldBe` "false"

      it "should generate sCrypt code for `IntLiteral` correctly" $ do
        r1 <- generateScryptFromIExpr $ Just $ IntM.IntLiteral True 15
        r1 `shouldBe` "0x0f"
        r2 <- generateScryptFromIExpr $ Just $ IntM.IntLiteral False 15
        r2 `shouldBe` "15"
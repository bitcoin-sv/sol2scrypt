module Transformations.Sol2IR.Expression where

import Solidity.Spec as Sol
import IR.Spec as IR
import IR.Transformer
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable Sol.Expression IExpr'" $ do
  it "should transfrom Solidity `BoolLiteral` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral "true")))
    r1 `shouldBe` Just (LiteralExpr $ IR.BoolLiteral True)
    r2 <- transform2IR TransformState (Literal (PrimaryExpressionBooleanLiteral (Sol.BooleanLiteral "false")))
    r2 `shouldBe` Just (LiteralExpr $ IR.BoolLiteral False)

  it "should transfrom Solidity `NumberLiteral` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Literal (PrimaryExpressionNumberLiteral (NumberLiteralHex "0123abcdef" Nothing)))
    r1 `shouldBe` Just (LiteralExpr $ IR.IntLiteral True 4893429231)
    r2 <- transform2IR TransformState (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "12345" Nothing)))
    r2 `shouldBe` Just (LiteralExpr $ IR.IntLiteral False 12345)

  it "should transfrom Solidity `HexLiteral` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Literal (PrimaryExpressionHexLiteral (HexLiteral "010113")))
    r1 `shouldBe` Just (LiteralExpr $ IR.BytesLiteral [01,01,19])

  it "should transfrom Solidity `Unary -` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Unary "-" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))))
    r1 `shouldBe` Just ( UnaryExpr Negate (LiteralExpr $ IR.IntLiteral False 100))

  it "should transfrom Solidity `Unary ()` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Unary "()" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))))
    r1 `shouldBe` Just ( Parens (LiteralExpr $ IR.IntLiteral False 100))



  it "should transfrom Solidity `Binary +` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Binary "+" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
    r1 `shouldBe` Just ( BinaryExpr IR.Add (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))

  it "should transfrom Solidity `Binary -` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Binary "-" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
    r1 `shouldBe` Just ( BinaryExpr IR.Sub (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))

  it "should transfrom Solidity `Binary *` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Binary "*" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
    r1 `shouldBe` Just ( BinaryExpr IR.Mul (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))

  it "should transfrom Solidity `Binary /` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Binary "/" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
    r1 `shouldBe` Just ( BinaryExpr IR.Div (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))

  it "should transfrom Solidity `Binary %` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Binary "%" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
    r1 `shouldBe` Just ( BinaryExpr IR.Mod (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))


  -- it "should transfrom Solidity `Binary +=` to IR Expression correctly" $ do
  --   r1 <- transform2IR TransformState (Binary "+=" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
  --   r1 `shouldBe` Just ( BinaryExpr IR.Add (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))

  -- it "should transfrom Solidity `Binary -=` to IR Expression correctly" $ do
  --   r1 <- transform2IR TransformState (Binary "-=" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
  --   r1 `shouldBe` Just ( BinaryExpr IR.Sub (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))

  -- it "should transfrom Solidity `Binary *=` to IR Expression correctly" $ do
  --   r1 <- transform2IR TransformState (Binary "*=" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
  --   r1 `shouldBe` Just ( BinaryExpr IR.Mul (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))

  -- it "should transfrom Solidity `Binary /=` to IR Expression correctly" $ do
  --   r1 <- transform2IR TransformState (Binary "/=" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
  --   r1 `shouldBe` Just ( BinaryExpr IR.Div (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))

  -- it "should transfrom Solidity `Binary %=` to IR Expression correctly" $ do
  --   r1 <- transform2IR TransformState (Binary "%=" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
  --   r1 `shouldBe` Just ( BinaryExpr IR.Mod (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))


  it "should transfrom Solidity `Binary ==` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Binary "==" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
    r1 `shouldBe` Just ( BinaryExpr IR.Equal (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))

  it "should transfrom Solidity `Binary !=` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Binary "!=" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
    r1 `shouldBe` Just ( BinaryExpr IR.Neq (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))

  it "should transfrom Solidity `Binary <` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Binary "<" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
    r1 `shouldBe` Just ( BinaryExpr IR.LessThan (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))

  it "should transfrom Solidity `Binary <=` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Binary "<=" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
    r1 `shouldBe` Just ( BinaryExpr IR.LessThanOrEqual (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))

  it "should transfrom Solidity `Binary >` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Binary ">" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
    r1 `shouldBe` Just ( BinaryExpr IR.GreaterThan (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))

  it "should transfrom Solidity `Binary >=` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Binary ">=" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
    r1 `shouldBe` Just ( BinaryExpr IR.GreaterThanOrEqual (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))

  it "should transfrom Solidity `Binary &&` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Binary "&&" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
    r1 `shouldBe` Just ( BinaryExpr IR.BoolAnd (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))

  it "should transfrom Solidity `Binary ||` to IR Expression correctly" $ do
    r1 <- transform2IR TransformState (Binary "||" (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "100" Nothing))) (Literal (PrimaryExpressionNumberLiteral (NumberLiteralDec "1" Nothing))))
    r1 `shouldBe` Just ( BinaryExpr IR.BoolOr (LiteralExpr $ IR.IntLiteral False 100) (LiteralExpr $ IR.IntLiteral False 1))
{-# LANGUAGE FlexibleContexts #-}

module Transformations.Sol2IR.Type where

import IR.Spec as IR
import IR.Transformer
import Solidity.Spec
import Test.Tasty
import Test.Tasty.Hspec

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable TypeName IType'" $ do
  let itType title e1 e2 = it ("should transfrom Solidity `" ++ title ++ "` to IR Type correctly") $ do
        r1 <- transform2IR (TransformState []) (TypeNameElementaryTypeName e1)
        r1 `shouldBe` Just (ElementaryType e2)

  describe "#TypeNameElementaryTypeName" $ do
    itType "BoolType" BoolType Bool

    itType "IntType" (IntType $ Just 8) Int

    itType "UintType" (UintType $ Just 256) Int

    itType "BytesType" (BytesType $ Just 1) Bytes

    itType "BytesType" ByteType Bytes

    itType "BytesType" VarType Any

    itType "BytesType" AddressType Address

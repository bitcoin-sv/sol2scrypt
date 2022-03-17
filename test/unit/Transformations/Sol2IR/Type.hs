{-# LANGUAGE FlexibleContexts #-}

module Transformations.Sol2IR.Type where

import IR.Spec as IR
import Test.Tasty
import Test.Tasty.Hspec
import Transformations.Helper

spec :: IO TestTree
spec = testSpec "instance ToIRTransformable TypeName IType'" $ do
  let itType sol e = it ("should transfrom Solidity `" ++ sol ++ "` to IR Type correctly") $ do
        r1 <- sol2Ir sol2Type sol
        r1 `shouldBe` Just (ElementaryType e)

  describe "#TypeNameElementaryTypeName" $ do
    itType "bool" Bool
    itType "int" Int
    itType "int8" Int
    itType "int256" Int
    itType "uint" Int
    itType "uint8" Int
    itType "uint256" Int
    itType "bytes" Bytes
    itType "byte" Bytes
    itType "var" Any
    itType "address" Address

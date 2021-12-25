{-# LANGUAGE FlexibleContexts #-}
module Transformations.Sol2IR.Variable where

import IR.Spec as IR
import IR.Transformer
import Solidity.Spec as Sol
import Test.Tasty
import Test.Tasty.Hspec
import Transformations.Helper

spec :: IO TestTree
spec = testSpec "Variables" $ do
  let itParameter sol t id = it ("should transfrom Solidity `" ++ sol ++ "` to IR Type correctly") $ do
        r1 <- sol2Ir sol2Parameter sol
        r1 `shouldBe` Just (IR.Param t id)
  describe "#Parameter" $ do
    itParameter "bool p" (ElementaryType Bool) (IR.Identifier "p")
    itParameter "int a" (ElementaryType Int) (IR.Identifier "a")
    itParameter "int8 a" (ElementaryType Int) (IR.Identifier "a")
    itParameter "int256 a" (ElementaryType Int) (IR.Identifier "a")
    itParameter "uint a" (ElementaryType Int) (IR.Identifier "a")
    itParameter "uint8 a" (ElementaryType Int) (IR.Identifier "a")
    itParameter "uint256 a" (ElementaryType Int) (IR.Identifier "a")
    itParameter "byte a" (ElementaryType Bytes) (IR.Identifier "a")
    itParameter "bytes a" (ElementaryType Bytes) (IR.Identifier "a")
    itParameter "bytes1 a" (ElementaryType Bytes) (IR.Identifier "a")
    itParameter "bytes32 a" (ElementaryType Bytes) (IR.Identifier "a")
    itParameter "address a" (ElementaryType Address) (IR.Identifier "a")

